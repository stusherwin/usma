import * as React from 'react';

export interface ButtonProps extends React.DetailedHTMLProps<React.ButtonHTMLAttributes<HTMLButtonElement>, HTMLButtonElement> {
  action: (e: React.SyntheticEvent) => void
  children?: any
}

export const Button = (props: ButtonProps) => {
  let {onClick, children, action, className, ...rest} = props

  return <button className={className} onClick={(e) => action(e)} {...rest}>{children}</button>
}