# Haskeract

Service for image processing built in Haskell.

## Dependencies

To run the project locally (in a Linux environment), make sure the following dependencies are installed:

- curl
- tesseract-ocr
- build-essential
- ca-certificates
- libffi-dev
- libffi8
- libgmp-dev
- libgmp10
- libncurses-dev
- libtinfo-dev
- libncursesw5-dev
- libssl-dev
- libsqlite3-dev
- tk-dev
- libgdbm-dev
- libc6-dev
- libbz2-dev
- zlib1g-dev
- libncurses5-dev
- libnss3-dev
- openssl
- libreadline-dev
- python3.11
- python3-pip

## Installing and running locally

To run **Haskeract** locally:

- Update the dependency repository so that packages from the [Hackage](https://hackage.haskell.org/) repository can be installed, using the command:

```bash
cabal update
```

- Install the dependencies for the features that use Python with the command

```bash
pip install -r requirements.txt
```

> It is recommended to create a virtual environment to isolate the project from the system.

- Finally, use the following command to start the server:

```bash
cabal run
```

> The project can also be run in a [Docker](https://docs.docker.com/get-started/) container. To do this, simply build an image from the Dockerfile and create a container from it.

## Endpoints

### GET /text

Extract text from an image via a URL.

**Request**:

- Method: `GET`
- URL: `/text`
- **Query Parameters**:
  - `url` (required, string): URL of an image available for download;

**Response**:

- Content: `text/plain`
- **Status Codes**:
  - `200 OK`: The image was downloaded and text was successfully extracted.
  - `400 BAD REQUEST`: No valid URL was provided.
  - `500 INTERNAL SERVER ERROR`: An error occurred during image download or text extraction.

### POST /text

Extract text from an uploaded image.

**Request**:

- Method: `POST`
- URL: `/text`
- **Form Data**:
  - `image` (required, bytes): Image from which the text will be extracted.

**Response**:

- Content: `text/plain`
- **Status Codes**:
  - `200 OK`: The text was successfully extracted.
  - `400 BAD REQUEST`: No image was provided.
  - `500 INTERNAL SERVER ERROR`: An error occurred during text extraction.

### GET /remove

Remove the background from an image via a URL.

**Request**:

- Method: `GET`
- URL: `/remove`
- **Query Parameters**:
  - `url` (required, string): URL of an image available for download.

**Response**:

- Content: `image/png`
- **Status Codes**:
  - `200 OK`: The image was downloaded and the background was successfully removed.
  - `400 BAD REQUEST`: No valid URL was provided.
  - `500 INTERNAL SERVER ERROR`: An error occurred during image download or background removal.

### POST /remove

Remove the background from an uploaded image.

**Request**:

- Method: `POST`
- URL: `/remove`
- **Form Data**:
  - `image` (required, bytes): Image from which the background will be removed.

**Response**:

- Content: `image/png`
- **Status Codes**:
  - `200 OK`: The background was successfully removed.
  - `400 BAD REQUEST`: No image was provided.
  - `500 INTERNAL SERVER ERROR`: An error occurred during background removal.

### POST /edit

Edit an image by applying filters or changing its dimensions.

**Request**:

- Method: `POST`
- URL: `/edit`
- **Form Data**:
  - `image` (required, bytes): Image to be edited.
  - `blur` (optional, number): Positive integer for blur intensity.
  - `height` (optional, number): Desired height in pixels.
  - `width` (optional, number): Desired width in pixels.
  - `laplacian` (optional, boolean): Edge detection.

**Response**:

- Content: `image/png`
- **Status Codes**:
  - `200 OK`: Filters were successfully applied.
  - `400 BAD REQUEST`: No image was provided.
  - `500 INTERNAL SERVER ERROR`: An error occurred during image processing.
