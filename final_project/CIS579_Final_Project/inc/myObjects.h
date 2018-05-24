#ifndef INC_MYOBJECTS_H_
#define INC_MYOBJECTS_H_

#include <stdint.h>

#define NUM_ALIENS (uint8_t)1
#define NUM_EXPLOSIONS (uint8_t)4
#define NUM_BULLETS (uint8_t)1
#define NUM_BOMBS (uint8_t)1

/* Spacecraft globals */
extern uint32_t spacecraftPosition[2];
extern uint32_t previousSpacecraftPosition[2];
extern uint8_t spacecraftIsDestroyed;

/* Bullet globals */
extern uint16_t bullets[NUM_BULLETS][2];
extern uint16_t previousBullets[NUM_BULLETS][2];
extern uint8_t currentBullet;

/* Fire button S2  value */
extern uint8_t currentFireButtonState;
extern uint8_t previousFireButtonState;

/* Aliens globals */
extern uint16_t aliens[NUM_ALIENS][2];
extern uint16_t previousAliens[NUM_ALIENS][2];

/* Bombs globals */
extern uint16_t bombs[NUM_BOMBS][2];
extern uint16_t previousbombs[NUM_BOMBS][2];
extern uint8_t currentBomb;
extern uint8_t bombTimerCounter;

/* Explosion globals */
enum
{
  x = 0,
  y = 1,
  enabled = 2, // Enable or not
  radius = 3, // Radius
  direction = 4  // Direction
};
extern uint16_t explosions[NUM_EXPLOSIONS][5];

/* Sound globals */
extern uint8_t pwmFireCounter;
extern uint8_t pwmExplosionCounter;

void initObjects(void);


#endif /* INC_MYOBJECTS_H_ */
