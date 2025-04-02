# Load the package
library(birdnetR)

# Initialize a BirdNET model
model <- birdnet_model_tflite()

# Path to the audio file (replace with your own file path)
audio_path <- system.file("extdata", "soundscape.wav", package = "birdnetR")

# Predict species within the audio file
predictions <- predict_species_from_audio_file(model, audio_path)

# Get most probable prediction within each time interval
get_top_prediction(predictions)
