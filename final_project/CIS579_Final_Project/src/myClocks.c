#include <driverlib.h>
#include "inc/myTimers.h"
#include "inc/myClocks.h"

void initClocks(void)
{
  // Set the core voltage level to handle a 25 MHz clock rate
  PMM_setVCore(PMM_CORE_LEVEL_3);

  // Set ACLK to use REFO as its oscillator source (32KHz)
  UCS_initClockSignal(UCS_ACLK,
  UCS_REFOCLK_SELECT,
                      UCS_CLOCK_DIVIDER_1);

  // Set REFO as the oscillator reference clock for the FLL
  UCS_initClockSignal(UCS_FLLREF,
  UCS_REFOCLK_SELECT,
                      UCS_CLOCK_DIVIDER_1);

  // Set MCLK and SMCLK to use the DCO/FLL as their oscillator source (25MHz)
  // The function does a number of things: Calculates required FLL settings; Configures FLL and DCO,
  // and then sets MCLK and SMCLK to use the DCO (with FLL runtime calibration)
  UCS_initFLLSettle(
      MCLK_SMCLK_DESIRED_FREQUENCY_IN_KHZ,
      (MCLK_SMCLK_DESIRED_FREQUENCY_IN_KHZ / (UCS_REFOCLK_FREQUENCY / 1024)));
}
