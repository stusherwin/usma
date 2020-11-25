import * as React from 'react';

export const Image = (props: { src: string, className?: string }) =>
  <img src={props.src} className={props.className} />