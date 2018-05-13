#include <driverlib.h>
#include "inc/myJoystickADC.h"
/**
 * @brief Enable the joystick analog to digital converter
 */
void initJoystickADC(void)
{

  /* TA0.1 is source ADC12_A_SAMPLEHOLDSOURCE_1 of this ADC */
  ADC12_A_init(
      ADC12_A_BASE,
      ADC12_A_SAMPLEHOLDSOURCE_1,
      ADC12_A_CLOCKSOURCE_SMCLK,
      ADC12_A_CLOCKDIVIDER_1);

  ADC12_A_enable(ADC12_A_BASE);

  // Change the resolution to be between 0 - 256 to work better with the
  // pixels on the LCD
  ADC12_A_setResolution(ADC12_A_BASE, ADC12_A_RESOLUTION_8BIT);

  ADC12_A_setupSamplingTimer(
      ADC12_A_BASE,
      ADC12_A_CYCLEHOLD_16_CYCLES,
      ADC12_A_CYCLEHOLD_16_CYCLES,
      ADC12_A_MULTIPLESAMPLESDISABLE);

  /* Joystick X memory input */
  ADC12_A_configureMemoryParam param0 =
  {
    ADC12_A_MEMORY_0,
    ADC12_A_INPUT_A5,
    ADC12_A_VREFPOS_AVCC,
    ADC12_A_VREFNEG_AVSS,
    ADC12_A_NOTENDOFSEQUENCE
  };

  ADC12_A_configureMemory(ADC12_A_BASE, &param0);

  /* Joystick Y memory input */
  ADC12_A_configureMemoryParam param1 =
  {
    ADC12_A_MEMORY_1,
    ADC12_A_INPUT_A3,
    ADC12_A_VREFPOS_AVCC,
    ADC12_A_VREFNEG_AVSS,
    ADC12_A_ENDOFSEQUENCE
  };

  ADC12_A_configureMemory(ADC12_A_BASE, &param1);

  // Enable memory buffer 1 interrupts
  // so interrupt fires when sequence mem0 -> mem1 or x -> y is completed
  ADC12_A_clearInterrupt(ADC12_A_BASE, ADC12IFG0);
  ADC12_A_clearInterrupt(ADC12_A_BASE, ADC12IFG1);
}

