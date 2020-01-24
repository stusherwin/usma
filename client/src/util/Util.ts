const months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
export class Util {
  static formatDate(date: Date): string {
    return `${date.getUTCDate()} ${months[date.getUTCMonth()]} ${date.getUTCFullYear()}`
  }

  static dateString(date: Date): string {
    const y = date.getUTCFullYear()
    const m = date.getUTCMonth() + 1
    const d = date.getUTCDate()
    return `${y}-${(m < 10 ? '0' : '') + m}-${(d < 10 ? '0' : '') + d}`
  }

  static formatMoney(pence: number, absolute: boolean): string {
    const amount = (absolute? Math.abs(pence) : pence) / 100.0
    
    return amount.toFixed(2)
  }

  static toTitleCase(str: string): string {
    return str.split(/\s+/).map(s => s.substr(0, 1).toUpperCase() + s.substr(1).toLowerCase()).join(' ');
  }

  static clone(obj: any): any {
    if(obj === null) {
      return null
    }
    
    if(typeof obj !== 'object') {
      return obj
    }

    let newObj: any = Array.isArray(obj)? [] : {}
    for(let prop in obj) {
      newObj[prop] = Util.clone(obj[prop])
    }
    return newObj
  }
}