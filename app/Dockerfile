FROM haskell:9.10-bullseye

# Define image arguments
ARG appHome=/app
ARG port=3000

# Install Tesseract OCR
RUN apt update && apt install -y tesseract-ocr

# Update the package list
RUN cabal update

# Create the project home directory
COPY . ${appHome}

# Set the working directory
WORKDIR ${appHome}

# Install the project
RUN cabal install

# Add installed cabal executables to PATH
ENV PATH=/root/.cabal/bin:$PATH

# Expose port
EXPOSE ${port}

# Set environment variables
ENV ENV=PROD
ENV PORT=${port}

# Execute the program
CMD ["haskeract"]