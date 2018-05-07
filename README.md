# levnor
Level normalization utility written in Haskell.
## Features
Takes image and transforms luminance to a normal distribution.
Works with grayscale images.

Grayscale input
![Grayscale in](./images/examples/grayscale.jpg)

Grayscale output
![Grayscale out](./images/examples/grayscale_out.bmp)

Also works with colored images.

Colored input
![Colored in](./images/examples/colored.jpg)

Colored output
![Colored out good](./images/examples/colored_out_good.bmp)

And some distribution parameters work worse than others

Colored output bad
![Colored out bad](./images/examples/colored_out_bad.bmp)

## Requirements
* Stack

## Building
```shell-script
git clone https://github.com/timecatler/levnor.git
cd levnor
stack install
```

Stack binaries location should be on your `PATH` for that to work.

## Usage

`levnor ./path_to_input_image.jpeg ./path_to_output_image.bmp median standart_deviation`