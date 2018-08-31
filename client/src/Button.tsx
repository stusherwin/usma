import * as React from 'react';
export interface ButtonProps extends React.DetailedHTMLProps<React.ButtonHTMLAttributes<HTMLButtonElement>, HTMLButtonElement> {
  action: (e: React.SyntheticEvent) => void
  children?: any
  icon?: string
}

export const Button = (props: ButtonProps) => {
  let {onClick, children, action, icon, className, ...rest} = props
  const iconImg = icon && <img src={`/img/${icon}.svg`} className="w-4 h-4" />

  if(!children) {
    className = (className || '') + " button-icon"
  }

  return <button className={className} onClick={(e) => action(e)} {...rest}>{iconImg}{children}</button>
}