#include <driverlib.h>

void initGPIO(void)
{
  /* Configures Pin 6.3 (Joystick Y) and 6.5 (Joystick X) as ADC input */
  GPIO_setAsInputPin(GPIO_PORT_P6, GPIO_PIN3);
  GPIO_setAsInputPin(GPIO_PORT_P6, GPIO_PIN5);

  /* Enable user button 2 on educational booster pack */
  GPIO_setAsInputPinWithPullUpResistor(GPIO_PORT_P3, GPIO_PIN7);

  /* Enable buzzer on educational booster pack with timer output going to this pin*/
  GPIO_setAsPeripheralModuleFunctionOutputPin(GPIO_PORT_P2, GPIO_PIN5);

  /* Enable P1.1 button to create interrupt to reset program */
  GPIO_setAsInputPinWithPullUpResistor( GPIO_PORT_P1, GPIO_PIN1 );
  GPIO_selectInterruptEdge ( GPIO_PORT_P1, GPIO_PIN1, GPIO_HIGH_TO_LOW_TRANSITION );
  GPIO_clearInterrupt ( GPIO_PORT_P1, GPIO_PIN1 );
  GPIO_enableInterrupt ( GPIO_PORT_P1, GPIO_PIN1 );

  /* Enable backlight pin for pwm adjustment */
  GPIO_setAsPeripheralModuleFunctionOutputPin(GPIO_PORT_P2, GPIO_PIN4);
}

