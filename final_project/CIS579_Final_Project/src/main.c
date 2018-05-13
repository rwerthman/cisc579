#include <msp430.h>
#include <driverlib.h>
#include <grlib.h>
#include "inc/Crystalfontz128x128_ST7735.h"
#include "inc/myTimers.h"
#include "inc/myClocks.h"
#include "inc/myGPIO.h"
#include "inc/myObjects.h"
#include "inc/myJoystickADC.h"
#include "inc/HAL_I2C.h"
#include "inc/ISRs.h"
#include "inc/main.h"

#include "math.h"

/* Utility functions */
uint32_t mapValToRange(uint32_t x, uint32_t input_min, uint32_t input_max,
                       uint32_t output_min, uint32_t output_max);

/* Graphics objects */
Graphics_Context g_sContext;
Graphics_Rectangle spacecraftRect;
Graphics_Rectangle bulletsRect;
Graphics_Rectangle explosionRect;
Graphics_Rectangle alienRect;
Graphics_Rectangle bombRect;

/**
 * main.c
 */
void main(void)
{

  // Stop the watchdog timer
  WDT_A_hold(WDT_A_BASE);

  initClocks();
  initTimers();
  initGPIO();
  initJoystickADC();
  initObjects();

  /* Initializes display */
  Crystalfontz128x128_Init();

  /* Set default screen orientation */
  Crystalfontz128x128_SetOrientation(LCD_ORIENTATION_UP);

  /* Initializes graphics context */
  Graphics_initContext(&g_sContext, &g_sCrystalfontz128x128);
  Graphics_setForegroundColor(&g_sContext, GRAPHICS_COLOR_GREEN);
  Graphics_setBackgroundColor(&g_sContext, GRAPHICS_COLOR_RED);
  GrContextFontSet(&g_sContext, &g_sFontFixed6x8);
  Graphics_clearDisplay(&g_sContext);

  /* Initialize I2C communication */
  Init_I2C_GPIO();
  I2C_init();

  // Globally enable interrupts
  __bis_SR_register(GIE);

  // Enable/Start first sampling and conversion cycle of the Joystick ADC
  ADC12_A_startConversion(ADC12_A_BASE, ADC12_A_MEMORY_0,
                          ADC12_A_REPEATED_SEQOFCHANNELS);

  // Start the timer to trigger the Joystick ADC and drawing of explosions
  Timer_A_startCounter(TIMER_A0_BASE, TIMER_A_UP_MODE);

  // Start the timer to trigger the Aliens, bullets, and bombs to be drawn
  Timer_A_startCounter(TIMER_A1_BASE, TIMER_A_UP_MODE);

  // Start the timer to handle the shooting and explosion sounds
  Timer_A_startCounter(TIMER_A2_BASE, TIMER_A_UP_MODE);

  while (1)
  {

  }

}

// https://stackoverflow.com/questions/5731863/mapping-a-numeric-range-onto-another
// https://github.com/arduino/Arduino/issues/2466
uint32_t mapValToRange(
    uint32_t x, uint32_t input_min, uint32_t input_max,
    uint32_t output_min, uint32_t output_max)
{
  uint32_t val = (uint32_t) (((x - input_min) * (
      output_max - output_min + 1) / (input_max - input_min + 1)) +
      output_min);
  return val;
}

void drawBullets(void)
{
  /* Read the "shoot" button input */
  currentFireButtonState = GPIO_getInputPinValue(GPIO_PORT_P3, GPIO_PIN7);
  /* Debounce the button until next interrupt */
  if (currentFireButtonState == previousFireButtonState &&
     currentFireButtonState == GPIO_INPUT_PIN_LOW)
  {
     /* If the button is low shoot a bullet */
     /* First store the bullets starting position */
     previousBullets[currentBullet][x] = bullets[currentBullet][x];
     previousBullets[currentBullet][y] = bullets[currentBullet][y];
     /* Set the bullet to the position of the spacecraft when
      * it was fired.
      */
     bullets[currentBullet][x] = spacecraftPosition[x];
     /* Subtract from the spacecraft position so the bullet looks like it is ahead of the
      * spacecraft when we shoot it and also we don't
      * clear the spacecraft when we clear the bullet
      */
     bullets[currentBullet][y] = spacecraftPosition[y] - 5;
     /* Advance to the next bullet or back to 0 if there isn't
      * another bullet
      */
     if (currentBullet < NUM_BULLETS)
     {
       currentBullet++;
     }
     else
     {
       currentBullet = 0;
     }
     /* Reset previous button state to be ready for the next button press */
     previousFireButtonState = GPIO_INPUT_PIN_HIGH;
     /* Generate PWM for the buzzer when shooting */
     Timer_A_setOutputMode(TIMER_A2_BASE,
                           TIMER_A_CAPTURECOMPARE_REGISTER_2,
                           TIMER_A_OUTPUTMODE_RESET_SET);
     pwmFireCounter = 0;
  }
  else
  {
     previousFireButtonState = currentFireButtonState;
     /* Let the pwm signal for the shot run for at least 3 context switches */
     if (pwmFireCounter < 3)
     {
       pwmFireCounter++;
     }
     else if (pwmFireCounter == 3)
     {
       /* Stop the PWM for shooting */
       Timer_A_setOutputMode(TIMER_A2_BASE,
                             TIMER_A_CAPTURECOMPARE_REGISTER_2,
                             TIMER_A_OUTPUTMODE_RESET);
       pwmFireCounter = 4;
     }

     /* If we aren't shooting and we can make the explosion noise */
     if (pwmExplosionCounter < 3 && pwmFireCounter == 4)
     {
       pwmExplosionCounter++;
     }
     /* If we aren't shooting and we are done with the explosion noises
      * turn of the PWM for the explosions
      * */
     else if (pwmFireCounter == 4 && pwmExplosionCounter == 3)
     {
       /* Stop the PWM for the explosions */
       Timer_A_setOutputMode(TIMER_A2_BASE,
                             TIMER_A_CAPTURECOMPARE_REGISTER_1,
                             TIMER_A_OUTPUTMODE_RESET);
       pwmExplosionCounter = 4;
     }
  }

  /* Advance all of the bullets by clearing their previous positions
   * and drawing them at their new positions */
  uint8_t i;
  for (i = 0; i < NUM_BULLETS ; i++)
  {
    /* If the bullets have been shot which means they aren't the
     * initial values they were set to draw them on the screen */
    if (bullets[i][x] != 200 && bullets[i][y] != 200)
    {
      /* Clear the bullets previous position */
      bulletsRect.xMax = previousBullets[i][x] + 2;
      bulletsRect.xMin = previousBullets[i][x] - 2;
      bulletsRect.yMax = previousBullets[i][y] + 2;
      bulletsRect.yMin = previousBullets[i][y] - 2;
      Graphics_fillRectangleOnDisplay(g_sContext.display, &bulletsRect,
                                      g_sContext.background);
      /* If the bullet has moved off the screen reset
       * it.
       */
      if (bullets[i][y] <= 0)
      {
        bullets[i][x] = 200;
        bullets[i][y] = 200;

      }
      else
      {
        /* Store the bullets position before moving the bullet
         * to a new position */
        previousBullets[i][x] = bullets[i][x];
        previousBullets[i][y] = bullets[i][y];
        /* Move the bullet up towards the top of the screen */
        bullets[i][y]--;

        /* Redraw the bullet at its new position */
        bulletsRect.xMax = bullets[i][x] + 2;
        bulletsRect.xMin = bullets[i][x] - 2;
        bulletsRect.yMax = bullets[i][y] + 2;
        bulletsRect.yMin = bullets[i][y] - 2;
        Graphics_setForegroundColor(&g_sContext, GRAPHICS_COLOR_BLACK);
        Graphics_fillRectangle(&g_sContext, &bulletsRect);
        Graphics_setForegroundColor(&g_sContext, GRAPHICS_COLOR_GREEN);
        /* Check if any of the aliens have been hit
         * by the bullet at its new position
         */
        uint8_t j;
        for (j = 0; j < NUM_ALIENS ; j++)
        {
          alienRect.xMax = aliens[j][x] + 2;
          alienRect.xMin = aliens[j][x] - 2;
          alienRect.yMax = aliens[j][y] + 2;
          alienRect.yMin = aliens[j][y] - 2;

          /* If a bullet and alien intersect... */
          if (Graphics_isRectangleOverlap(&bulletsRect, &alienRect))
          {
            /* Draw an explosion at the alien and bullet  */
            explosions[j][x] = aliens[j][x];
            explosions[j][y] = aliens[j][y];
            explosions[j][radius] = 5;
            explosions[j][enabled] = 1;
            explosions[j][direction] = 1;
            /* Generate PWM for the buzzer to make a sound of an explosion */
            Timer_A_setOutputMode(TIMER_A2_BASE,
                                  TIMER_A_CAPTURECOMPARE_REGISTER_1,
                                  TIMER_A_OUTPUTMODE_RESET_SET);
            pwmExplosionCounter = 0;
            /* Clear the aliens position */
            alienRect.xMax = aliens[j][x] + 2;
            alienRect.xMin = aliens[j][x] - 2;
            alienRect.yMax = aliens[j][y] + 2;
            alienRect.yMin = aliens[j][y] - 2;
            Graphics_fillRectangleOnDisplay(g_sContext.display, &alienRect,
                                            g_sContext.background);
            /* Reset the alien */
            aliens[j][x] = 200;
            aliens[j][y] = 200;
            previousAliens[j][x] = 200;
            previousAliens[j][y] = 200;

            /* Clear the bullets position */
            bulletsRect.xMax = bullets[i][x] + 2;
            bulletsRect.xMin = bullets[i][x] - 2;
            bulletsRect.yMax = bullets[i][y] + 2;
            bulletsRect.yMin = bullets[i][y] - 2;
            Graphics_setForegroundColor(&g_sContext, GRAPHICS_COLOR_BLACK);
            Graphics_fillRectangleOnDisplay(g_sContext.display, &bulletsRect,
                                            g_sContext.background);
            Graphics_setForegroundColor(&g_sContext, GRAPHICS_COLOR_GREEN);

            /* Reset the bullet */
            bullets[i][x] = 200;
            bullets[i][y] = 200;
            previousBullets[i][x] = bullets[i][x];
            previousBullets[i][y] = bullets[i][y];
          }
        }
      }
    }
  }
}

void drawExplosions(void)
{
  uint8_t i;
  /* Advance the explosions */
  for (i = 0; i < NUM_EXPLOSIONS ; i++)
  {
    /* If the explosion has been set to be shown */
    if (explosions[i][enabled])
    {
      /* If the explosion is set to increase in direction */
      if (explosions[i][direction] == 1)
      {
        /* Don't need to clear previous circle because the explosion gets bigger */
        Graphics_setForegroundColor(&g_sContext, GRAPHICS_COLOR_PURPLE);
        Graphics_fillCircle(&g_sContext, explosions[i][x], explosions[i][y],
                            explosions[i][radius]);
        Graphics_setForegroundColor(&g_sContext, GRAPHICS_COLOR_GREEN);
        explosions[i][radius] += 5;
        /* Max explosion radius should be 10 but we have to go to 15
         * to draw a circle with radius 10 */
        if (explosions[i][radius] >= 15)
        {
          explosions[i][direction] = 0;
          explosions[i][radius] = 10;
        }
      }
      else
      {
        /* Clear previous explosion circle because circle gets smaller */
        explosionRect.xMax = explosions[i][x] + explosions[i][radius];
        explosionRect.xMin = explosions[i][x] - explosions[i][radius];
        explosionRect.yMax = explosions[i][y] + explosions[i][radius];
        explosionRect.yMin = explosions[i][y] - explosions[i][radius];
        Graphics_setForegroundColor(&g_sContext, GRAPHICS_COLOR_PURPLE);
        Graphics_fillRectangleOnDisplay(g_sContext.display, &explosionRect,
                                        g_sContext.background);
        Graphics_setForegroundColor(&g_sContext, GRAPHICS_COLOR_GREEN);
        /* Decrease the explosion size */
        explosions[i][radius] -= 5;
        /* Minimum explosion size is 0.  Stop the explosion
         * when it gets to that size
         */
        if (explosions[i][radius] <= 0)
        {
          /* Don't draw circle with radius 0 */
          explosions[i][enabled] = 0;
        }
        else
        {
          /* Otherwise keep drawing smaller explosions */
          Graphics_setForegroundColor(&g_sContext, GRAPHICS_COLOR_PURPLE);
          Graphics_fillCircle(&g_sContext, explosions[i][x],
                              explosions[i][y],
                              explosions[i][radius]);
          Graphics_setForegroundColor(&g_sContext, GRAPHICS_COLOR_GREEN);
        }
      }
    }
  }
}

void drawAliens(void)
{
  /* Draw the aliens */
  uint8_t i;
  for (i = 0; i < NUM_ALIENS ; i++)
  {
    /* If the aliens aren't dead... */
    if (aliens[i][x] != 200 && aliens[i][y] != 200)
    {
      /* Clear the aliens previous position */
      alienRect.xMax = previousAliens[i][x] + 2;
      alienRect.xMin = previousAliens[i][x] - 2;
      alienRect.yMax = previousAliens[i][y] + 2;
      alienRect.yMin = previousAliens[i][y] - 2;
      Graphics_fillRectangleOnDisplay(g_sContext.display, &alienRect,
                                      g_sContext.background);
      /* Keep the aliens on the screen */
      /* Alien is moving to the right almost off the screen */
      if (aliens[i][x] > 125 && previousAliens[i][x] < aliens[i][x])
      {
        previousAliens[i][x] = aliens[i][x];
        aliens[i][x]--;
      }
      /* Alien is moving to the left almost off the screen */
      else if (aliens[i][x] < 2 && previousAliens[i][x] > aliens[i][x])
      {
        previousAliens[i][x] = aliens[i][x];
        aliens[i][x]++;
      }
      /* Alien is just moving to the right */
      else if (aliens[i][x] > previousAliens[i][x])
      {
        /* Keep alien moving to the right */
        previousAliens[i][x] = aliens[i][x];
        aliens[i][x]++;
      }
      /* Alien is just moving to the left */
      else if (aliens[i][x] < previousAliens[i][x])
      {
        /* Keep alien moving to the left */
        previousAliens[i][x] = aliens[i][x];
        aliens[i][x]--;
      }
      /* Alien hasn't started moving */
      else if (aliens[i][x] == previousAliens[i][x])
      {
        /* Choose a direction based on i */
        if (i % 2)
        {
          // To the right
          previousAliens[i][x] = aliens[i][x];
          aliens[i][x]++;
        }
        else
        {
          // To the left
          previousAliens[i][x] = aliens[i][x];
          aliens[i][x]--;
        }
      }
      else
      {
        /* This shouldn't be reached.
         * There shouldn't be any other way for the alien
         * to move.
         */
        while (1);
      }
      /* Draw the alien at its new position */
      alienRect.xMax = aliens[i][x] + 2;
      alienRect.xMin = aliens[i][x] - 2;
      alienRect.yMax = aliens[i][y] + 2;
      alienRect.yMin = aliens[i][y] - 2;
      Graphics_setForegroundColor(&g_sContext, GRAPHICS_COLOR_YELLOW);
      Graphics_fillRectangle(&g_sContext, &alienRect);
      Graphics_setForegroundColor(&g_sContext, GRAPHICS_COLOR_GREEN);
    }
    else
    {
      /* Don't draw the alien because it is dead */
    }
  }
}

void drawBombs(void)
{
  uint8_t i;
  /* Interrupt should fire every 10ms so count up to 1 second
   * before firing another bomb.  So bombs are fired every 1 second */
  if (bombTimerCounter > 99)
  {
    bombTimerCounter = 0;
    if (bombs[currentBomb][x] == 200 && bombs[currentBomb][y] == 200)
    {
      /* If we haven't shot the bombs, shoot the bombs from the aliens position */
      bombs[currentBomb][x] = aliens[currentBomb % 3][x];
      bombs[currentBomb][y] = aliens[currentBomb % 3][y];
    }

    /* Go to the next bomb */
    if (currentBomb < (NUM_BOMBS - 1))
    {
      currentBomb++;
    }
    else
    {
      currentBomb = 0;
    }
  }
  else
  {
    bombTimerCounter++; /* Count up to a 100 (1 second) before shooting a bomb */
    for (i = 0; i < NUM_BOMBS; i++)
    {
      /* Draw the bombs if they aren't equal to their default values */
      if (bombs[i][x] != 200 && bombs[i][y] != 200)
      {
        /* Store the bombs position */
        previousbombs[i][x] = bombs[i][x];
        previousbombs[i][y] = bombs[i][y];

        /* Erase the bombs position */
        bombRect.xMax = bombs[i][x] + 2;
        bombRect.xMin = bombs[i][x] - 2;
        bombRect.yMax = bombs[i][y] + 2;
        bombRect.yMin = bombs[i][y] - 2;
        Graphics_fillRectangleOnDisplay(g_sContext.display, &bombRect,
                                       g_sContext.background);

        /* Advance the bomb down the screen */
        bombs[i][y] += 3;

        /* Draw the bomb at the new position */
        bombRect.xMax = bombs[i][x] + 2;
        bombRect.xMin = bombs[i][x] - 2;
        bombRect.yMax = bombs[i][y] + 2;
        bombRect.yMin = bombs[i][y] - 2;
        Graphics_setForegroundColor(&g_sContext, GRAPHICS_COLOR_BROWN);
        Graphics_fillRectangle(&g_sContext, &bombRect);
        Graphics_setForegroundColor(&g_sContext, GRAPHICS_COLOR_GREEN);

        /* Check if bombs are off screen */
        if (bombs[i][y] > 127)
        {
          /* Erase the bombs position */
          bombRect.xMax = bombs[i][x] + 2;
          bombRect.xMin = bombs[i][x] - 2;
          bombRect.yMax = bombs[i][y] + 2;
          bombRect.yMin = bombs[i][y] - 2;
          Graphics_fillRectangleOnDisplay(g_sContext.display, &bombRect,
                                     g_sContext.background);
          /* Reset the bombs */
          bombs[i][x] = 200;
          bombs[i][y] = 200;
        }
        else
        {
          /* Check if the bombs and the spaceship collide */
          bombRect.xMax = bombs[i][x] + 2;
          bombRect.xMin = bombs[i][x] - 2;
          bombRect.yMax = bombs[i][y] + 2;
          bombRect.yMin = bombs[i][y] - 2;

          spacecraftRect.xMax = spacecraftPosition[x] + 2;
          spacecraftRect.xMin = spacecraftPosition[x] - 2;
          spacecraftRect.yMax = spacecraftPosition[y] + 2;
          spacecraftRect.yMin = spacecraftPosition[y] - 2;

          if (Graphics_isRectangleOverlap(&bombRect, &spacecraftRect))
          {
            /* Draw an explosion at the spacecraft and bomb  */
            explosions[3][x] = spacecraftPosition[x];
            explosions[3][y] = spacecraftPosition[y];
            explosions[3][radius] = 5;
            explosions[3][enabled] = 1;
            explosions[3][direction] = 1;

            /* Generate PWM for the buzzer to make a sound of an explosion */
            Timer_A_setOutputMode(TIMER_A2_BASE,
                                  TIMER_A_CAPTURECOMPARE_REGISTER_1,
                                  TIMER_A_OUTPUTMODE_RESET_SET);
            pwmExplosionCounter = 0;

            /* Clear the spacecrafts previous position */
            spacecraftRect.xMax = spacecraftPosition[x] + 2;
            spacecraftRect.xMin = spacecraftPosition[x] - 2;
            spacecraftRect.yMax = spacecraftPosition[y] + 2;
            spacecraftRect.yMin = spacecraftPosition[y] - 2;
            Graphics_fillRectangleOnDisplay(g_sContext.display,
                                            &spacecraftRect,
                                            g_sContext.background);

            /* Stop drawing the spacecraft */
            spacecraftIsDestroyed = 1;
            spacecraftPosition[x] = 300;
            spacecraftPosition[y] = 300;
            previousSpacecraftPosition[x] = spacecraftPosition[x];
            previousSpacecraftPosition[y] = spacecraftPosition[y];

            /* Erase the bombs position */
            bombRect.xMax = bombs[i][x] + 2;
            bombRect.xMin = bombs[i][x] - 2;
            bombRect.yMax = bombs[i][y] + 2;
            bombRect.yMin = bombs[i][y] - 2;
            Graphics_fillRectangleOnDisplay(
                g_sContext.display, &bombRect, g_sContext.background);

            /* Reset the bombs */
            bombs[i][x] = 200;
            bombs[i][y] = 200;

            /* Store the bombs new position */
            previousbombs[i][x] = bombs[i][x];
            previousbombs[i][y] = bombs[i][y];
          }
        }
      }
    }
  }
}

void drawSpacecraft(void)
{
  if (!spacecraftIsDestroyed)
  {
    /* Store the spacecrafts previous position */
    previousSpacecraftPosition[x] = spacecraftPosition[x];
    previousSpacecraftPosition[y] = spacecraftPosition[y];
    /* Store the spacecrafts new position */
    spacecraftPosition[x] = mapValToRange(
        (uint32_t) ADC12_A_getResults(ADC12_A_BASE, ADC12_A_MEMORY_0),
        0UL, 255UL, 0UL, 127UL);
    /* Invert the y values so when the joystick goes up y goes down */
    spacecraftPosition[y] = 0x7F & ~mapValToRange(
        (uint32_t) ADC12_A_getResults(ADC12_A_BASE, ADC12_A_MEMORY_1),
        0UL, 255UL, 0UL, 127UL);
    /* If the spacecraft is in a new position clear its old position and redraw it */
    if (previousSpacecraftPosition[x] != spacecraftPosition[x]
        || previousSpacecraftPosition[y] != spacecraftPosition[y])
    {
      /* Clear the spacecrafts previous position */
      spacecraftRect.xMax = previousSpacecraftPosition[x] + 2;
      spacecraftRect.xMin = previousSpacecraftPosition[x] - 2;
      spacecraftRect.yMax = previousSpacecraftPosition[y] + 2;
      spacecraftRect.yMin = previousSpacecraftPosition[y] - 2;
      Graphics_fillRectangleOnDisplay(g_sContext.display, &spacecraftRect,
                                      g_sContext.background);
      /* Draw it's new position */
      spacecraftRect.xMax = spacecraftPosition[x] + 2;
      spacecraftRect.xMin = spacecraftPosition[x] - 2;
      spacecraftRect.yMax = spacecraftPosition[y] + 2;
      spacecraftRect.yMin = spacecraftPosition[y] - 2;
      Graphics_fillRectangle(&g_sContext, &spacecraftRect);
    }
  }
}
