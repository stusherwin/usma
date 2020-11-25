import * as React from 'react';

export const Image = (props: { src: string, className?: string }) => {
  return <img src="data:image/png;base64,R0lGODlhAQABAAD/ACwAAAAAAQABAAACADs=" data-src={props.src} className={props.className} />
} 