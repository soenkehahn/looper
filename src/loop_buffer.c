#include <stdio.h>
#include <stdlib.h>
#include <jack/jack.h>

jack_port_t *output_port1, *output_port2;

struct buffer {
  float* array;
  int length;
  int index;
};

int process(jack_nframes_t nframes, void* arg) {
  struct buffer* buffer = arg;

  jack_default_audio_sample_t* out1, * out2;
  out1 = (jack_default_audio_sample_t*) jack_port_get_buffer(output_port1, nframes);
  out2 = (jack_default_audio_sample_t*) jack_port_get_buffer(output_port2, nframes);

  for(int i = 0; i < nframes; i++) {
    float output = buffer->array[buffer->index];
    out1[i] = output;
    out2[i] = output;

    buffer->index++;
    if (buffer->index >= buffer->length) {
      buffer->index = 0;
    }
  }

  return 0;
}

void jack_shutdown(void *arg) {
  fprintf(stderr, "jack_shutdown");
  exit(1);
}

jack_client_t* init_client() {
  const char *client_name = "foo";
  const char *server_name = NULL;
  jack_options_t options = JackNullOption;
  jack_status_t status;

  jack_client_t *client;
  client = jack_client_open(client_name, options, &status, server_name);
  if (client == NULL) {
    fprintf(stderr, "jack_client_open() failed, status = 0x%2.0x\n", status);
    if (status & JackServerFailed) {
      fprintf(stderr, "Unable to connect to JACK server\n");
    }
    exit(1);
  }

  if (status & JackServerStarted) {
    fprintf(stderr, "JACK server started\n");
  }

  if (status & JackNameNotUnique) {
    client_name = jack_get_client_name(client);
    fprintf(stderr, "unique name `%s' assigned\n", client_name);
  }

  jack_on_shutdown(client, jack_shutdown, 0);

  return client;
}

struct buffer* loop_buffer(float* array, int length) {
  struct buffer* buffer = malloc(sizeof(buffer));
  buffer->array = array;
  buffer->length = length;
  buffer->index = 0;

  jack_client_t* client = init_client();

  jack_set_process_callback(client, process, buffer);

  output_port1 = jack_port_register(client, "output1",
    JACK_DEFAULT_AUDIO_TYPE,
    JackPortIsOutput, 0);

  output_port2 = jack_port_register(client, "output2",
    JACK_DEFAULT_AUDIO_TYPE,
    JackPortIsOutput, 0);

  if ((output_port1 == NULL) || (output_port2 == NULL)) {
    fprintf(stderr, "no more JACK ports available\n");
    exit(1);
  }

  if (jack_activate(client)) {
    fprintf(stderr, "cannot activate client");
    exit(1);
  }

  return buffer;
}

void set_buffer(struct buffer* buffer, float* array, int length) {
  buffer->array = array;
  buffer->length = length;
  buffer->index = 0;
}
