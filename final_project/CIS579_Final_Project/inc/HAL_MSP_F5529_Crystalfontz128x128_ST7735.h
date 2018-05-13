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
// HAL_MSP_F5529_Crystalfontz128x128_ST7735.h -
//           Hardware abstraction layer for using the Educational Boosterpack's
//           Crystalfontz128x128 LCD with MSP-F5529 LaunchPad
//
//*****************************************************************************

#ifndef __HAL_MSP_F5529_CRYSTALFONTZLCD_H_
#define __HAL_MSP_F5529_CRYSTALFONTZLCD_H_


#include <stdint.h>
#include <driverlib/MSP430F5xx_6xx/driverlib.h>
//*****************************************************************************
//
// User Configuration for the LCD Driver
//
//*****************************************************************************

// System clock speed (in Hz)
#define LCD_SYSTEM_CLOCK_SPEED                 24000000
// SPI clock speed (in Hz)
#define LCD_SPI_CLOCK_SPEED                    24000000

// Ports from MSP432 connected to LCD
#define LCD_SCK_PORT          GPIO_PORT_P3
#define LCD_MOSI_PORT         GPIO_PORT_P3
#define LCD_RST_PORT          GPIO_PORT_P7
#define LCD_CS_PORT           GPIO_PORT_P2
#define LCD_DC_PORT           GPIO_PORT_P8

// Pins from MSP432 connected to LCD
#define LCD_SCK_PIN           GPIO_PIN2
#define LCD_MOSI_PIN          GPIO_PIN0
#define LCD_RST_PIN           GPIO_PIN4
#define LCD_CS_PIN            GPIO_PIN6
#define LCD_DC_PIN            GPIO_PIN2

// Definition of USCI base address to be used for SPI communication
#define LCD_USCI_BASE        USCI_B0_BASE

//*****************************************************************************
//
// Prototypes for the globals exported by this driver.
//
//*****************************************************************************
extern void HAL_LCD_writeCommand(uint8_t command);
extern void HAL_LCD_writeData(uint8_t data);
extern void HAL_LCD_PortInit(void);
extern void HAL_LCD_SpiInit(void);

#define HAL_LCD_delay(x)      __delay_cycles(x * 48)

#endif /* HAL_MSP_F5529_CRYSTALFONTZ128X128_ST7735_H_ */
