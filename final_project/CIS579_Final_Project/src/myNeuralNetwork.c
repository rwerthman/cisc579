#include "inc/myNeuralNetwork.h"
#include <stdio.h>

#define learning_rate 0.5
float weights_input_to_hidden[num_hidden][num_inputs];
float weights_hidden_to_output[num_outputs][num_hidden];

float sigmoid_function(float x);

// https://stackoverflow.com/questions/4310277/producing-random-float-from-negative-to-positive-range
void initialize_neural_network_weights()
{
  uint8_t i, j;

  /* Initialize the input to hidden layer weights */
  for (i = 0; i < num_hidden; i++)
  {
    for (j = 0; j < num_inputs; j++)
    {
      // Weights are initialized to 1/sqrt(number of links to nodes) in the positive to negative range
      weights_input_to_hidden[i][j] = (
          ((float)rand() / RAND_MAX) * (2*(1.0/sqrtf(num_inputs))))
          - 1.0/sqrtf(num_inputs);
    }
  }

  /* Initialize the hidden to output layer weights */
  for (i = 0; i < num_outputs; i++)
    {
      for (j = 0; j < num_hidden; j++)
      {
        // Weights are initialized to 1/sqrt(number of links to nodes) in the positive to negative range
        weights_hidden_to_output[i][j] = (
            ((float)rand() / RAND_MAX) * (2*(1.0/sqrtf(num_hidden))))
            - 1.0/sqrtf(num_hidden);
      }
    }


}
/**
 * Multiple the inputs by the weights and then put them through the
 * activation function (sigmoid) to get an output between 0 and 1.
 */
void calculate_neural_network_outputs(float output[num_outputs], uint8_t input[num_inputs])
{
  uint8_t i, j;
  float hidden_output[num_hidden];

  // Calculate dot product of the weights and input for the input to
  // the hidden layer
  for (i = 0; i < num_hidden; i++)
  {
    for (j = 0; j < num_inputs; j++)
    {
      hidden_output[i] += input[j] * weights_input_to_hidden[i][j];
    }
  }

  // Put the product of weights and inputs through activation function
  // to get the output of the hidden layer.  This is the input
  // to the output layer
  for (i = 0; i < num_hidden; i++)
  {
    hidden_output[i] = sigmoid_function(hidden_output[i]);
  }

  // Calculate dot product of the weights and input for the hidden to
  // the output layer
  for (i = 0; i < num_outputs; i++)
  {
    for (j = 0; j < num_hidden; j++)
    {
      output[i] += hidden_output[j] * weights_hidden_to_output[i][j];
    }
  }

  // Put the product of weights and hidden outputs through
  // the sigmoid activation function
  // to get the output of the output layer.  This is the prediction
  // of based on the inputs to the network
  for (i = 0; i < num_outputs; i++)
  {
    output[i] = sigmoid_function(output[i]);
  }
}

/**
 * Determine the error between the expected output and actual output
 * and take that error and propagate it back to the weights in the
 * neural network.
 */
void backpropagate_error_to_weights(uint8_t input[num_inputs], float actual_output[num_outputs], uint8_t expected_outputs[num_outputs])
{
  uint8_t i, j;
  float output_error[num_outputs];
  float hidden_error[num_hidden];
  float hidden_output[num_hidden];

  // Calculate the error for the output layer
  for (i = 0; i < num_outputs; i++)
  {
    /* Derivative of squared error function .5(x - y)^2 */
    output_error[i] = (expected_outputs[i] - actual_output[i]);

  }

  /* Multiply the output layer error by the hidden-to-output weights to
   * get the hidden layer error */
  for (i = 0; i < num_outputs; i++)
  {
    for (j = 0; j < num_hidden; j++)
    {
      hidden_error[j] += weights_hidden_to_output[i][j] * output_error[i];
    }
  }

  for (i = 0; i < num_outputs; i++)
  {
    output_error[i] = output_error[i] * actual_output[i] * (1 - actual_output[i]);
  }

  /* Calculate the hidden outputs from the input-to-hidden weights,
   * the inputs from input layer to hidden layers, and the sigmoid function */
  for (i = 0; i < num_hidden; i++)
  {
    for (j = 0; j < num_inputs; j++)
    {
      hidden_output[i] += input[j] * weights_input_to_hidden[i][j];
    }
  }

  // Put the product of weights and inputs through activation function
  // to get the output of the hidden layer.  This is the input
  // to the output layer
  for (i = 0; i < num_hidden; i++)
  {
    hidden_output[i] = sigmoid_function(hidden_output[i]);
  }

  /* Update the hidden-to-output layer weights */
  for (i = 0; i < num_outputs; i++)
  {
    for (j = 0; j < num_hidden; j++)
    {
      weights_hidden_to_output[i][j] += learning_rate * output_error[i] * hidden_output[j];
    }
  }

  /* Update the input-to-hidden layer weights */
  for (i = 0; i < num_hidden; i++)
  {
    hidden_error[i] = hidden_error[i] * hidden_output[i] * (1 - hidden_output[i]);
  }

  for (i = 0; i < num_hidden; i++)
  {
    for (j = 0; j < num_inputs; j++)
    {
      weights_input_to_hidden[i][j] += learning_rate * hidden_error[i] * input[j];
    }
  }

}

float sigmoid_function(float x)
{
  return 1/(1 + expf(-x));
}

void generate_training_data(uint16_t num_training_samples)
{
  uint16_t i, num_left, num_right, num_predicted_left, num_predicted_right;
  uint8_t x, y, z;

  uint8_t training_input[num_inputs];
  uint8_t training_output[num_outputs];
  float real_output[num_outputs];

  num_right = 0;
  num_left = 0;
  num_predicted_left = 0;
  num_predicted_right = 0;

  for (i = 0; i < num_training_samples; i++)
  {

    // Randomly choose the x locations of the bullet, alien, and spaceship
    x = rand() % 127; // location of bullet
    y = rand() % 127; // location of alien
    z = rand() % 127; // location of spaceship

    training_input[0] = x;
    training_input[1] = y;
    training_input[2] = z;

    /* Reset the training output and read output */
    training_output[0] = 0; // If 0, move left, else if 1, move right

    real_output[0] = 0;

    // Based on the training input decide what the alien should do
    // x = bullet, y = alien, z = spaceship
    /* Move alien to the right if spaceship is right to go towards the spaceship
     * or left if the bullet is left to avoid the bullet */
    if (y < z)
    {
      training_output[0] = 0;
      num_right++;
    }
    /* Move alien to the left if spaceship is left to go towards the
     * spaceship or right if the bullet is right */
    else if (y > z)
    {
      training_output[0] = 1;
      num_left++;
    }

    calculate_neural_network_outputs(real_output, training_input);

    if (real_output[0] <= .5 && training_output[0] == 0)
    {
      num_predicted_right++;
    }
    else if (real_output[0] > .5 && training_output[0] == 1)
    {
      num_predicted_left++;
    }

    backpropagate_error_to_weights(training_input, real_output, training_output);

  }

  printf("Num left %d\n", num_left);
  printf("Num right %d\n", num_right);

  printf("Num predicted left %d\n", num_predicted_left);
  printf("Num predicted right %d\n", num_predicted_right);

}


