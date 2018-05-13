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
//*****************************************************************************
//
// HAL_MSP_F5529_Crystalfontz128x128_ST7735.c -
//           Hardware abstraction layer for using the Educational Boosterpack's
//           Crystalfontz128x128 LCD with MSP-F5529 LaunchPad
//
//*****************************************************************************

#include <grlib/grlib.h>
#include <driverlib/MSP430F5xx_6xx/driverlib.h>
#include <inc/HAL_MSP_F5529_Crystalfontz128x128_ST7735.h>
#include <stdint.h>

void HAL_LCD_PortInit(void)
{
    // LCD_SCK
    GPIO_setAsPeripheralModuleFunctionOutputPin(LCD_SCK_PORT, LCD_SCK_PIN);
    // LCD_MOSI
    GPIO_setAsPeripheralModuleFunctionOutputPin(LCD_MOSI_PORT, LCD_MOSI_PIN);
    // LCD_RST
    GPIO_setAsOutputPin(LCD_RST_PORT, LCD_RST_PIN);
    // LCD_RS
    GPIO_setAsOutputPin(LCD_DC_PORT, LCD_DC_PIN);
    // LCD_CS
    GPIO_setAsOutputPin(LCD_CS_PORT, LCD_CS_PIN);
}

void HAL_LCD_SpiInit(void)
{
    USCI_B_SPI_initMasterParam config =
        {
            USCI_B_SPI_CLOCKSOURCE_SMCLK,
            UCS_getSMCLK(),
            UCS_getSMCLK(),
            USCI_B_SPI_MSB_FIRST,
            USCI_B_SPI_PHASE_DATA_CAPTURED_ONFIRST_CHANGED_ON_NEXT,
            USCI_B_SPI_CLOCKPOLARITY_INACTIVITY_LOW
        };
    USCI_B_SPI_initMaster(LCD_USCI_BASE, &config);
    USCI_B_SPI_enable(LCD_USCI_BASE);

    GPIO_setOutputLowOnPin(LCD_CS_PORT, LCD_CS_PIN);

    GPIO_setOutputHighOnPin(LCD_DC_PORT, LCD_DC_PIN);
}


//*****************************************************************************
//
// Writes a command to the CFAF128128B-0145T.  This function implements the basic SPI
// interface to the LCD display.
//
//*****************************************************************************
void HAL_LCD_writeCommand(uint8_t command)
{
    // Set to command mode
    GPIO_setOutputLowOnPin(LCD_DC_PORT, LCD_DC_PIN);

    // USCI_B0 Busy? //
    //while (UCB0STATW & UCBUSY);
    while ( USCI_B_SPI_isBusy(LCD_USCI_BASE) );

    // Transmit data
    //UCB0TXBUF = command;
    USCI_B_SPI_transmitData( LCD_USCI_BASE , command );

    // USCI_B0 Busy? //
    //while (UCB0STATW & UCBUSY);
    while ( USCI_B_SPI_isBusy(LCD_USCI_BASE) );

    // Set back to data mode
    GPIO_setOutputHighOnPin(LCD_DC_PORT, LCD_DC_PIN);
}


//*****************************************************************************
//
// Writes a data to the CFAF128128B-0145T.  This function implements the basic SPI
// interface to the LCD display.
//
//*****************************************************************************
void HAL_LCD_writeData(uint8_t data)
{
    // USCI_B0 Busy? //
    //while (UCB0STATW & UCBUSY);
    while ( USCI_B_SPI_isBusy(LCD_USCI_BASE) );

    // Transmit data
    //UCB0TXBUF = data;
    USCI_B_SPI_transmitData( LCD_USCI_BASE , data );

    // USCI_B0 Busy? //
    //while (UCB0STATW & UCBUSY);
    while ( USCI_B_SPI_isBusy(LCD_USCI_BASE) );
}
