#include "inc/ISRs.h"
#include "msp430f5529.h"
#include "main.h"

#pragma vector=TIMER0_A1_VECTOR
__interrupt void TimerA0ISR(void)
{
  switch (__even_in_range(TA0IV, TA0IV_TACCR2))
  {
    case TA0IV_TACCR1: /* capture compare register 1 interrupt flag for timer a1 */
    {
      drawSpacecraft(); /* Draw the spaceship */
      break;
    }
    case TA0IV_TACCR2: /* capture compare register 2 interrupt flag for timer a1 */
    {
      drawExplosions(); /* Draw explosions when bombs and bullets strike aliens and spaceship */
      break;
    }
  }
}

#pragma vector=TIMER1_A0_VECTOR
__interrupt void TimerA1CCR0ISR(void)
{
  drawAliens(); /* Draw the aliens */
}

#pragma vector=TIMER1_A1_VECTOR
__interrupt void TimerA1ISR(void)
{
  switch (__even_in_range(TA1IV, TA1IV_TACCR2))
  {
    case TA1IV_TACCR1: /* capture compare register 1 interrupt flag for timer a1 */
    {
      drawBullets(); /* Draw the spaceship bullets */
      break;
    }
    case TA1IV_TACCR2: /* capture compare register 2 interrupt flag for timer a1 */
    {
      drawBombs(); /* Draw the alien bombs */
      break;
    }
  }
}

#pragma vector=PORT1_VECTOR
__interrupt void pushButtonISR(void)
{
  switch (__even_in_range(P1IV, P1IV_P1IFG1))
  {
    case P1IV_P1IFG1:
      // WDTCTL = 0xDEAD; // watchdog timer password violation causes a power up clear reset
      PMMCTL0 = PMMPW | PMMSWBOR; // Software brown out reset
      break;
    default:
      break;
  }
}
