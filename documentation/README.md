# Midgard Documentation

This directory contains the documentation website for Midgard, built with [Nextra](https://nextra.site).

## Development

To start the development server:

```bash
# Install dependencies
pnpm install

# Start development server
pnpm dev
```

The site will be available at [http://localhost:3000](http://localhost:3000).

## Structure

```
docs/
├── app/           # Next.js app directory
├── content/       # Documentation content (MDX files)
├── components/    # React components
├── public/        # Static assets
└── styles/        # CSS styles
```

## Adding Content

Documentation content is written in MDX and stored in the `content` directory. The sidebar navigation is automatically generated from the file structure.
