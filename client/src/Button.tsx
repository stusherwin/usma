import * as React from 'react';

export const Button = (props: {action: (e: React.SyntheticEvent) => void, disabled?: boolean, children: any}) =>
  <button disabled={props.disabled} onClick={(e) => props.action(e)}>{props.children}</button>