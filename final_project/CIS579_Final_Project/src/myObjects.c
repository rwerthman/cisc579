#include "inc/myObjects.h"

/* Sound globals */
uint8_t pwmFireCounter = 4;
uint8_t pwmExplosionCounter = 4;

/* Spacecraft globals */
uint32_t spacecraftPosition[2];
uint32_t previousSpacecraftPosition[2];
uint8_t spacecraftIsDestroyed = 0;

/* Bullet globals */
uint16_t bullets[NUM_BULLETS][2];
uint16_t previousBullets[NUM_BULLETS][2];
uint8_t currentBullet = 0;
/* Fire button S2  value */
uint8_t currentFireButtonState;
uint8_t previousFireButtonState;

/* Aliens globals */
uint16_t aliens[NUM_ALIENS][2];
uint16_t previousAliens[NUM_ALIENS][2];

/* Explosion globals */
uint16_t explosions[NUM_EXPLOSIONS][5];

/* Bomb globals */
uint16_t bombs[NUM_BOMBS][2];
uint16_t previousbombs[NUM_BOMBS][2];
uint8_t currentBomb = 0;
uint8_t bombTimerCounter = 0;

void initObjects(void)
{
  uint8_t i;
  for (i = 0; i < NUM_BULLETS ; i++)
  {
    bullets[i][x] = 200;
    bullets[i][y] = 200;
    previousBullets[i][x] = bullets[i][x];
    previousBullets[i][y] = bullets[i][y];
  }

  for (i = 0; i < NUM_ALIENS; i++)
  {
    /* Put the aliens in different places on the screen */
    aliens[i][x] = 10 + 15 * i;
    aliens[i][y] = 10 + 15 * i;
    previousAliens[i][x] = aliens[i][x];
    previousAliens[i][y] = aliens[i][y];
  }

  for (i = 0; i < NUM_BOMBS; i++)
  {
    bombs[i][x] = 200;
    bombs[i][y] = 200;
    previousbombs[i][x] = bombs[i][x];
    previousbombs[i][y] = bombs[i][y];
  }

  for (i = 0; i < NUM_EXPLOSIONS; i++)
  {
    /* Disable the explosions */
    explosions[i][enabled] = 0;
  }

  spacecraftPosition[x] = 0;
  spacecraftPosition[y] = 0;

  previousSpacecraftPosition[x] = spacecraftPosition[x];
  previousSpacecraftPosition[y] = spacecraftPosition[y];
}

