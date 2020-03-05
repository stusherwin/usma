import * as React from 'react';

import { Util } from './Util'

export interface MoneyProps {
  amount: number
  absolute?: boolean
  noColour?: boolean
  oppositeColours?: boolean
  className?: string
}

export const Money = ({amount, absolute, noColour, oppositeColours, className}: MoneyProps) => {
  absolute = absolute || false
  className = (!absolute && !noColour && amount < 0 ? (oppositeColours? 'text-green-dark ' : 'text-red ') : (oppositeColours? 'text-red ' : ' ')) + className
  
  return <span className={className}>{!absolute && amount < 0 && '-'}&pound;{ Util.formatMoney(amount, true) }</span>
}