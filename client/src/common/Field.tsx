import * as React from 'react';
import * as classNames from 'classnames'
import { Field } from './Validation'

export interface TextFieldProps {
  id: string
  label: string
  field: Field
  valueOnChange: (value: string) => void
}

export const TextField = (props: TextFieldProps) => {
  const onChange = (event: React.ChangeEvent<HTMLInputElement>) =>
    props.valueOnChange(event.target.value)

  return (
    <div className={classNames('field mb-4', {'invalid': !props.field.valid})}>
      <div className="flex justify-between items-baseline">
        <label className="flex-no-grow flex-no-shrink mr-2"
               htmlFor={props.id}>{props.label}:</label>
        <input type="text" 
               id={props.id} 
               autoFocus
               className="input flex-grow flex-no-shrink"
               value={props.field.stringValue} 
               onChange={onChange} />
      </div>
      <div className="text-red mt-2" hidden={!props.field.error}>
        {props.field.error}
      </div>
    </div>
  )
}

export interface MoneyFieldProps {
  id: string
  label: string
  field: Field
  valueOnChange: (value: string) => void
}

export const MoneyField = (props: MoneyFieldProps) => {
  const onChange = (event: React.ChangeEvent<HTMLInputElement>) =>
    props.valueOnChange(event.target.value)

  return (
    <div className={classNames('field mb-4', {'invalid': !props.field.valid})}>
      <div className="flex justify-between items-baseline">
        <label className="flex-no-grow flex-no-shrink mr-1"
               htmlFor={props.id}>{props.label}:<span className="ml-2">&pound;</span></label>
        <input type="text"
               id={props.id}
               className="input flex-grow flex-no-shrink"
               value={props.field.stringValue}
               onChange={onChange} />
      </div>
      <div className="text-red mt-2" hidden={!props.field.error}>
        {props.field.error}
      </div>
    </div>
  )
}

export interface DropDownFieldProps {
  id: string
  label: string
  field: Field
  valueOnChange: (value: string) => void
  options: (string | {name: string, value: string})[]
}

export const DropDownField = (props: DropDownFieldProps) => {
  const onChange = (event: React.ChangeEvent<HTMLSelectElement>) =>
    props.valueOnChange(event.target.value)
  
  return (
    <div className={classNames('field mb-4', {'invalid': !props.field.valid})}>
      <div className="flex justify-between items-baseline">
        <label className="flex-no-grow flex-no-shrink mr-2"
               htmlFor={props.id}>{props.label}:</label>
        <select id={props.id} 
                className="flex-grow flex-no-shrink"
                value={props.field.stringValue}
                onChange={onChange}>
          {props.options.map(o => typeof o === 'string'
            ? <option key={o} value={o}>{o}</option>
            : <option key={o.value} value={o.value}>{o.name}</option>
          )}
        </select>
      </div>
      <div className="text-red mt-2" hidden={!props.field.error}>
        {props.field.error}
      </div>
    </div>
  )
}
