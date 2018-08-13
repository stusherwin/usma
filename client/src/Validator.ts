export interface FieldValidation {
  validate: (stringValue: string) => boolean
  error: string
}

export interface Field {
  defaultValue: any
  stringValue: string
  value: any
  valid: boolean
  error: string | null
  validation: FieldValidation[]
  parse: (stringValue: string) => any
}

export interface Form {
  validating: boolean
  valid: boolean 
  fields: {[key: string]: Field}
}

export class Validator {
  static field(defaultValue: any, parse: (stringValue: string) => any, validation: FieldValidation[]): Field {
    return { stringValue: ''
           , defaultValue
           , value: defaultValue
           , validation
           , parse
           , valid: true
           , error: null
           }
  }
  
  static resetField(field: Field): Field {
    return { stringValue: ''
           , value: field.defaultValue
           , defaultValue: field.defaultValue
           , validation: field.validation
           , parse: field.parse
           , valid: true
           , error: null
           }
  }
  
  static validateField(field: Field): Field {
    let error = null
    let valid = true
    for(let v of field.validation) {
      if(!v.validate(field.stringValue)) {
        valid = false
        error = v.error
        break
      }
    }

    return { stringValue: field.stringValue
           , value: field.value
           , defaultValue: field.defaultValue
           , validation: field.validation
           , parse: field.parse
           , valid
           , error
           }
  }

  static updateField(field: Field, stringValue: string): Field {
    let error = null
    let valid = true
    for(const v of field.validation) {
      if(!v.validate(stringValue)) {
        valid = false
        error = v.error
        break
      }
    }

    const value = field.parse(stringValue)

    return { stringValue
           , value: value
           , defaultValue: field.defaultValue
           , validation: field.validation
           , parse: field.parse
           , valid
           , error
           }
  }

  static cloneField(field: Field): Field {
    return { stringValue: field.stringValue
           , value: field.value
           , defaultValue: field.defaultValue
           , validation: field.validation
           , parse: field.parse
           , valid: field.valid
           , error: field.error
           }
  }

  static form(fields: {[key: string]: Field}): Form {
    return { validating: false
           , valid: true
           , fields: fields
           }
  }

  static reset(form: Form): Form {
    const fields: {[key: string]: Field} = {}
    for(let f in form.fields) {
      fields[f] = this.resetField(form.fields[f])
    }

    return { validating: false
           , valid: true
           , fields
           }
  }

  static all(fields: {[key: string]: Field}, predicate: (f: Field) => boolean): boolean {
    let result = true
    for(let f in fields) {
      result = result && predicate(fields[f])
    }
    
    return result
  }

  static map(fields: {[key: string]: Field}, fn: (f: Field) => Field): {[key: string]: Field} {
    let result: {[key: string]: Field} = {}
    for(let f in fields) {
      result[f] = fn(fields[f])
    }
    return result
  }

  static validate(form: Form): Form {
    const fields = this.map(form.fields, this.validateField)

    return { validating: true
           , valid: this.all(fields, f => f.valid)
           , fields
           }
  }

  static update(form: Form, fieldName: string, stringValue: string) {
    const fields = this.map(form.fields, this.cloneField)
    fields[fieldName] = this.updateField(form.fields[fieldName], stringValue)
    
    return { validating: form.validating
           , valid: this.all(fields, f => f.valid)
           , fields
           }
    
  }
}