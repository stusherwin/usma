import * as React from 'react';

import { Util } from './Util'

export class Money extends React.Component<{amount: number}, {}> {
  render() {
    return <span>&pound;{ Util.formatMoney(this.props.amount) }</span>
  }
}