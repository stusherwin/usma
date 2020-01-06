import * as React from 'react';

import { ProductFlags } from './FilteredProducts'
import { Icon } from '../util/Icon'
import { Util } from '../util/Util'

export interface ProductFiltersProps { searchString: string
                                     , flags: ProductFlags
                                     , categories: string[]
                                     , brands: string[]
                                     , searchChanged: (value: string) => void
                                     , flagChanged: (changedFlag: string) => void
                                     , categoryChanged: (changedCategory: string | null) => void
                                     , brandChanged: (changedBrand: string | null) => void
                                     }

export const ProductFilters = ({searchString, flags, categories, brands, searchChanged, flagChanged, categoryChanged, brandChanged}: ProductFiltersProps) => (
  <div className="bg-product-light p-2">
    <label htmlFor="search" className="text-white">Search for a particular product:</label>
    <div className="relative mt-2">
      <span className="absolute text-grey-darker" style={{bottom: '0px', left: '4px'}}><Icon type="search" className="w-4 h-4 fill-current" /></span>
      <input type="text" id="search" placeholder="e.g. 'FX109' or 'Oat Bran'" autoFocus className="w-full input icon" value={searchString} onChange={e => searchChanged(e.target.value)} />
    </div>
    <div className="relative mt-2 flex space-between">
        <select className="mr-1" style={{flexBasis: '50%'}} onChange={e => categoryChanged(e.target.value)}>
          <option key="" value="">All categories</option>
          {categories.filter(c => !/^\s*$/.test(c)).map(c => 
            <option key={c} value={c}>{Util.toTitleCase(c.replace(';', ','))}</option>
          )}
        </select>
        <select className="ml-1" style={{flexBasis: '50%'}} onChange={e => brandChanged(e.target.value)}>
          <option key="" value="">All brands</option>
          {brands.filter(b => !/^\s*$/.test(b)).map(b => 
            <option key={b} value={b}>{Util.toTitleCase(b.replace(';', ','))}</option>
          )}
        </select>
    </div>
    <div className="mt-2 flex justify-between">
      <span className="whitespace-no-wrap" style={{flexBasis: '33.3%'}}>
        <input type="checkbox" id="b" value="b" checked={flags['b']} className="mr-1 nudge-d-1" onChange={e => flagChanged(e.target.value)} />
        <label htmlFor="b" className="text-white"><span className="inline-block text-center w-4 h-4" style={{ marginRight: 3, color: '#992f83', backgroundColor: '#eeabe0' }}>
          <span className="inline-block">B</span></span>iodynamic
        </label>
        </span>
      <span className="whitespace-no-wrap ml-4" style={{flexBasis: '33.3%'}}>
        <input type="checkbox" id="g" value="g" checked={flags['g']} className="mr-1 nudge-d-1" onChange={e => flagChanged(e.target.value)} />
        <label htmlFor="g" className="text-white"><span className="inline-block text-center w-4 h-4" style={{ marginRight: 3, color: '#992f83', backgroundColor: '#eeabe0' }}>
          <span className="inline-block">G</span></span>luten Free
        </label>
      </span>
      <span className="whitespace-no-wrap ml-4" style={{flexBasis: '33.3%'}}>
        <input type="checkbox" id="o" value="o" checked={flags['o']} className="mr-1 nudge-d-1" onChange={e => flagChanged(e.target.value)} />
        <label htmlFor="o" className="text-white"><span className="inline-block text-center w-4 h-4" style={{ marginRight: 3, color: '#992f83', backgroundColor: '#eeabe0' }}>
          <span className="inline-block">O</span></span>rganic
        </label>
      </span>
    </div>
    <div className="mt-1 flex justify-between">
      <span className="whitespace-no-wrap" style={{flexBasis: '33.3%'}}>
        <input type="checkbox" id="f" value="f" checked={flags['f']} className="mr-1 nudge-d-1" onChange={e => flagChanged(e.target.value)} />
        <label htmlFor="f" className="text-white"><span className="inline-block text-center w-4 h-4" style={{ marginRight: 3, color: '#992f83', backgroundColor: '#eeabe0' }}>
          <span className="inline-block">F</span></span>air Trade
        </label>
      </span>
      <span className="whitespace-no-wrap ml-4" style={{flexBasis: '33.3%'}}>
        <input type="checkbox" id="v" value="v" checked={flags['v']} className="mr-1 nudge-d-1" onChange={e => flagChanged(e.target.value)} />
        <label htmlFor="v" className="text-white"><span className="inline-block text-center w-4 h-4" style={{ marginRight: 3, color: '#992f83', backgroundColor: '#eeabe0' }}>
          <span className="inline-block">V</span></span>egan
        </label>
      </span>
      <span className="whitespace-no-wrap ml-4" style={{flexBasis: '33.3%'}}>
        <input type="checkbox" id="s" value="s" checked={flags['s']} className="mr-1 nudge-d-1" onChange={e => flagChanged(e.target.value)} />
        <label htmlFor="s" className="text-white">Added <span className="inline-block text-center w-4 h-4" style={{ marginRight: 3, color: '#992f83', backgroundColor: '#eeabe0' }}>
          <span className="inline-block">S</span></span>ugar
        </label>
      </span>
    </div>
  </div>
)
