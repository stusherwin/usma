import * as React from 'react';

import { Util } from './Util'

export interface MoneyProps {
  amount: number
  absolute?: boolean
  noColour?: boolean
  className?: string
}

export const Money = ({amount, absolute, noColour, className}: MoneyProps) => {
  absolute = absolute || false
  className = (!absolute && !noColour && amount < 0 ? 'text-red ' : '') + className
  
  return <span className={className}>{!absolute && amount < 0 && '-'}&pound;{ Util.formatMoney(amount, true) }</span>
}