FROM ubuntu:latest

# Define image arguments
ARG appHome=/app
ARG port=3000
ARG pythonVersion=3.11
ARG ghcVersion=9.2.8
ARG cabalVersion=3.8.1.0

# Add repositories for the runtimes
RUN apt update -y && apt install -y --no-install-recommends software-properties-common
RUN add-apt-repository ppa:deadsnakes/ppa

# Install system dependencies
RUN apt update -y && apt install -y --no-install-recommends \
    curl \
    tesseract-ocr \
    build-essential \
    ca-certificates \
    libffi-dev \
    libffi8 \
    libgmp-dev \
    libgmp10 \
    libncurses-dev \
    libtinfo-dev \
    libncursesw5-dev \
    libssl-dev \
    libsqlite3-dev \
    tk-dev \
    libgdbm-dev \
    libc6-dev \
    libbz2-dev \
    zlib1g-dev \
    libncurses5-dev \
    libnss3-dev \
    openssl \
    libreadline-dev \
    python${pythonVersion} \
    python3-pip

# Install GHCup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
ENV PATH=/root/.ghcup/bin:$PATH

# Install GHC and Cabal
RUN mkdir -p /root/.ghcup/ghc
RUN ghcup install ghc --set --force ${ghcVersion} 
RUN ghcup install cabal --set --force ${cabalVersion} 

# Download models for background removal
RUN mkdir -p /root/.u2net
RUN curl -o /root/.u2net/u2net.onnx https://github.com/danielgatis/rembg/releases/download/v0.0.0/u2net.onnx

# Clean up
RUN apt autoclean && apt autoremove && rm -rf /var/lib/apt/lists/*

# Create the project home directory
COPY . ${appHome}

# Set the working directory
WORKDIR ${appHome}

# Install python dependencies
RUN python${pythonVersion} -m pip install -r requirements.txt --ignore-installed

# Update the package list
RUN cabal update

# Install the project
RUN cabal install

# Add installed cabal executables to PATH
ENV PATH=/root/.cabal/bin:$PATH

# Expose port
EXPOSE ${port}

# Set environment variables
ENV LANG=C.UTF-8
ENV ENV=PROD
ENV PORT=${port}

# Execute the program
CMD ["haskeract"]