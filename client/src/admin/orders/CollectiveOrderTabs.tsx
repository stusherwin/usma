import * as React from 'react';
import * as classNames from 'classnames'

export interface CollectiveOrderTabsProps {
  tab: 'households' | 'product-list' | 'product-codes'
  setTab: (tab: 'households' | 'product-list' | 'product-codes') => void
}

export const CollectiveOrderTabs = ({tab, setTab}: CollectiveOrderTabsProps) => 
  <div className="flex justify-end items-baseline">
    <div className="flex justify-end rounded-sm items-baseline border border-order-darker p-1 bg-white shadow-sm-inner-top">
      <a href="#" className={classNames("border rounded-sm whitespace-no-wrap px-2 py-1 no-underline hover:no-underline", {
          "bg-white border-white text-black hover:text-black": tab == 'households', 
          "bg-grey-light border-grey text-grey-darkest hover:bg-grey hover:border-grey-dark hover:text-black": tab != 'households' 
        })} onClick={e => { e.preventDefault(); e.stopPropagation(); setTab('households'); }}>Households</a>
      <a href="#" className={classNames("ml-1 border rounded-sm whitespace-no-wrap px-2 py-1 no-underline hover:no-underline", {
          "bg-white border-white text-black hover:text-black": tab == 'product-list', 
          "bg-grey-light border-grey text-grey-darkest hover:bg-grey hover:border-grey-dark hover:text-black": tab != 'product-list' 
        })} onClick={e => { e.preventDefault(); e.stopPropagation(); setTab('product-list'); }}>Product list</a>
      <a href="#" className={classNames("ml-1 border rounded-sm whitespace-no-wrap px-2 py-1 no-underline hover:no-underline", {
          "bg-white border-white text-black hover:text-black": tab == 'product-codes', 
          "bg-grey-light border-grey text-grey-darkest hover:bg-grey hover:border-grey-dark hover:text-black": tab != 'product-codes' 
        })} onClick={e => { e.preventDefault(); e.stopPropagation(); setTab('product-codes'); }}>Product codes</a>
    </div>
  </div> 