#include <stdio.h>
#include <stdlib.h>
#include <jack/jack.h>

jack_port_t *output_port1, *output_port2;
jack_client_t *client;

float* g_buffer;
int g_length;
int g_index;

int process(jack_nframes_t nframes, void* arg) {
  jack_default_audio_sample_t* out1, * out2;

  out1 = (jack_default_audio_sample_t*) jack_port_get_buffer(output_port1, nframes);
  out2 = (jack_default_audio_sample_t*) jack_port_get_buffer(output_port2, nframes);

  for(int i = 0; i < nframes; i++) {
    float output = g_buffer[g_index];
    out1[i] = output;
    out2[i] = output;

    g_index++;
    if (g_index >= g_length) {
      g_index = 0;
    }
  }

  return 0;
}

void jack_shutdown(void *arg) {
  fprintf(stderr, "jack_shutdown");
  exit(1);
}

void loop_buffer(float* buffer, int length) {
  g_buffer = buffer;
  g_length = length;
  g_index = 0;

  const char *client_name = "foo";
  const char *server_name = NULL;
  jack_options_t options = JackNullOption;
  jack_status_t status;

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

  jack_set_process_callback(client, process, NULL);

  jack_on_shutdown(client, jack_shutdown, 0);

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
}
