/* --COPYRIGHT--,BSD
 * Copyright (c) 2015, Texas Instruments Incorporated
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * *  Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * *  Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * *  Neither the name of Texas Instruments Incorporated nor the names of
 *    its contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * --/COPYRIGHT--*/
//****************************************************************************
//
// HAL_I2C.c - Hardware abstraction layer for I2C with MSP432P401R
//
//****************************************************************************

#include <driverlib/MSP430F5xx_6xx/driverlib.h>
#include "HAL_I2C.h"

void Init_I2C_GPIO()
{
    /* Select I2C function for I2C_SCL(P4.2) & I2C_SDA(P4.1) */
    GPIO_setAsPeripheralModuleFunctionOutputPin(
            GPIO_PORT_P4,
            GPIO_PIN2);

    GPIO_setAsPeripheralModuleFunctionOutputPin(
            GPIO_PORT_P4,
            GPIO_PIN1);
}


/***************************************************************************//**
 * @brief  Configures I2C
 * @param  none
 * @return none
 ******************************************************************************/

void I2C_init(void)
{
  /* I2C Master Configuration Parameter */
  USCI_B_I2C_initMasterParam i2cConfig =
  {
    USCI_B_I2C_CLOCKSOURCE_SMCLK,
    UCS_getSMCLK(),
    USCI_B_I2C_SET_DATA_RATE_100KBPS
  };
  /* Initialize USCI_B1 and I2C Master to communicate with slave devices*/
  USCI_B_I2C_initMaster(USCI_B1_BASE, &i2cConfig);

    /* Disable I2C module to make changes */
  USCI_B_I2C_disable(USCI_B1_BASE);

  /* Enable I2C Module to start operations */
  USCI_B_I2C_enable(USCI_B1_BASE);

    return;
}


/***************************************************************************//**
 * @brief  Reads data from the sensor
 * @param  writeByte Address of register to read from
 * @return Register contents
 ******************************************************************************/

int I2C_read16(unsigned char writeByte)
{
    volatile int val = 0;
    volatile int valScratch = 0;

    /* Set master to transmit mode PL */
    USCI_B_I2C_setMode(USCI_B1_BASE,
                       USCI_B_I2C_TRANSMIT_MODE);

    /* Clear any existing interrupt flag PL */
    USCI_B_I2C_clearInterrupt(USCI_B1_BASE,
                              USCI_B_I2C_TRANSMIT_INTERRUPT +
                              USCI_B_I2C_RECEIVE_INTERRUPT);

    __no_operation();
    __disable_interrupt();
    __no_operation();

    /* Clear Rx buffer to make room for next receive*/
    USCI_B_I2C_masterReceiveMultiByteNext(USCI_B1_BASE);

    /* Wait until ready to write PL */
    while (USCI_B_I2C_isBusBusy(USCI_B1_BASE));

    /* Initiate start and send first character */
    USCI_B_I2C_masterSendMultiByteStart(USCI_B1_BASE, writeByte);

    /* Wait for TX to finish */
    while(!(USCI_B_I2C_getInterruptStatus(USCI_B1_BASE,
                                          USCI_B_I2C_TRANSMIT_INTERRUPT)));

    /* Initiate stop only */
    USCI_B_I2C_masterSendMultiByteStop(USCI_B1_BASE);

    /* Wait for Stop to finish */
    while(!USCI_B_I2C_getInterruptStatus(USCI_B1_BASE,
                                         USCI_B_I2C_TRANSMIT_INTERRUPT));
    /*
     * Generate Start condition and set it to receive mode.
     * This sends out the slave address and continues to read
     * until you issue a STOP
     */

    //Initialize multi reception
    USCI_B_I2C_masterReceiveMultiByteStart(USCI_B1_BASE);

    /* Wait for RX buffer to fill */
    while(!(USCI_B_I2C_getInterruptStatus(USCI_B1_BASE,
                                          USCI_B_I2C_RECEIVE_INTERRUPT)));

    /* Read from I2C RX register */
    val = USCI_B_I2C_masterReceiveMultiByteNext(USCI_B1_BASE);

    /* Receive second byte then send STOP condition */
    valScratch = USCI_B_I2C_masterReceiveMultiByteFinish(USCI_B1_BASE);
//    __no_operation();
//    __enable_interrupt();
//    __no_operation();

    /* Shift val to top MSB */
    val = (val << 8);

    /* Read from I2C RX Register and write to LSB of val */
    val |= valScratch;

    /* Return temperature value */
    return (int16_t)val;
}


/***************************************************************************//**
 * @brief  Writes data to the sensor
 * @param  pointer  Address of register you want to modify
 * @param  writeByte Data to be written to the specified register
 * @return none
 ******************************************************************************/

void I2C_write16 (unsigned char pointer, unsigned int writeByte)
{
  /* Set master to transmit mode PL */
  USCI_B_I2C_setMode(USCI_B1_BASE,
                   USCI_B_I2C_TRANSMIT_MODE);

  /* Clear any existing interrupt flag PL */
  USCI_B_I2C_clearInterrupt(USCI_B1_BASE, USCI_B_I2C_TRANSMIT_INTERRUPT);

  /* Wait until ready to write */
  while (USCI_B_I2C_isBusBusy(USCI_B1_BASE));

  /* Initiate start and send first character */
  USCI_B_I2C_masterSendMultiByteStart(USCI_B1_BASE, pointer);

  /* Send the MSB to SENSOR */
  USCI_B_I2C_masterSendMultiByteNext(USCI_B1_BASE,
      (unsigned char)(writeByte>>8));

  USCI_B_I2C_masterSendMultiByteFinish(USCI_B1_BASE,
      (unsigned char)(writeByte&0xFF));
}


void I2C_setslave(unsigned int slaveAdr)
{
  /* Specify slave address for I2C */
  USCI_B_I2C_setSlaveAddress(USCI_B1_BASE, slaveAdr);

    /* Enable and clear the interrupt flag */
  USCI_B_I2C_clearInterrupt(USCI_B1_BASE,
                            USCI_B_I2C_TRANSMIT_INTERRUPT +
                            USCI_B_I2C_RECEIVE_INTERRUPT);

    return;
}
