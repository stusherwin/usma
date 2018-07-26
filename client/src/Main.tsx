import * as React from 'react';

export interface MainProps {}
export interface MainState {}

export class Main extends React.Component<MainProps, MainState> {
  constructor(props: MainProps) {
    super(props)

    this.state = {}
  }

  componentDidMount() {
  }

  render() {
    return (
      <div>
        Hi there
      </div>
    )
  }
}