#include <portaudio.h>
#include <stdio.h>
#include <stdlib.h>

struct looper {
  float* array;
  int index;
  int length;
  struct looper* next;
};

struct looper* create_empty_looper() {
  struct looper* result = malloc(sizeof(struct looper));
  result->array = NULL;
  result->index = 0;
  result->length = 0;
  result->next = NULL;
  return result;
}

void set_buffer(struct looper* looper, float* array, int length) {
  struct looper* next = malloc(sizeof(struct looper));
  next->array = array;
  next->index = 0;
  next->length = length;
  next->next = NULL;
  looper->next = next;
}

float get_next_sample(struct looper* looper) {
  if (looper->index >= looper->length && looper->next != NULL) {
    looper->array = looper->next->array;
    looper->index = looper->next->index;
    looper->length = looper->next->length;
    looper->next = NULL;
  }
  if (looper->length == 0) {
    return 0.0;
  }
  if (looper->index >= looper->length) {
    looper->index = 0;
  }
  float sample = looper->array[looper->index];
  looper->index++;
  return sample;
}

void start_portaudio(struct looper* looper);

struct looper* create_looper() {
  struct looper* looper = create_empty_looper();
  start_portaudio(looper);
  return looper;
}

// portaudio stuff

static int paCallback(
  const void* inputBuffer,
  void* outputBuffer,
  unsigned long framesPerBuffer,
  const PaStreamCallbackTimeInfo* timeInfo,
  PaStreamCallbackFlags statusFlags,
  void* userdata
) {
  struct looper* looper = (struct looper*) userdata;
  float* out = (float*) outputBuffer;

  for(unsigned long i = 0; i < framesPerBuffer; i++) {
    float sample = get_next_sample(looper);
    out[0] = sample; // left
    out[1] = sample; // right
    out += 2;
  }

  return paContinue;
}

void start_portaudio(struct looper* looper) {
  PaStreamParameters outputParameters;
  PaStream* stream;
  PaError err;

  err = Pa_Initialize();
  if(err != paNoError) goto error;

  outputParameters.device = Pa_GetDefaultOutputDevice();
  if (outputParameters.device == paNoDevice) {
    fprintf(stderr,"Error: No default output device.\n");
    goto error;
  }
  outputParameters.channelCount = 2;
  outputParameters.sampleFormat = paFloat32;
  outputParameters.suggestedLatency =
    Pa_GetDeviceInfo(outputParameters.device)->defaultLowOutputLatency;
  outputParameters.hostApiSpecificStreamInfo = NULL;

  err = Pa_OpenStream(
    &stream,
    NULL,
    &outputParameters,
    44100,
    1024,
    paClipOff,
    paCallback,
    looper
  );
  if(err != paNoError) goto error;

  err = Pa_StartStream(stream);
  if(err != paNoError) goto error;

  return;

error:
  Pa_Terminate();
  fprintf(stderr, "An error occured while using the portaudio stream\n");
  fprintf(stderr, "Error number: %d\n", err);
  fprintf(stderr, "Error message: %s\n", Pa_GetErrorText(err));
}
