import * as React from 'react';

export type OrderTab = 'households' | 'product-list' | 'product-codes'

export interface OrderTabsProps {
  tab: OrderTab | undefined
  householdsBg: string
  productsBg: string
  productCodesBg: string
  setTab: (tab: OrderTab) => void
}

export const OrderTabs = ({tab, setTab, householdsBg, productsBg, productCodesBg}: OrderTabsProps) => 
    <div className="flex -ml-2 -mr-2 -mb-4">
      <a href="#" className={"relative pt-2 pb-3 flex-grow text-center rounded-tr-lg mr-1 text-black no-underline hover:text-black hover:underline flex justify-center items-center"
        + ((tab || 'households') == 'households' ? ` ${householdsBg} shadow-inner-top` : '')} onClick={e => { e.preventDefault(); e.stopPropagation(); setTab('households'); }}>
          {/* <div className="bg-no-repeat w-6 h-6 mr-2 bg-img-household desaturate" ></div> */}
          <span>Households</span>
      </a>
      <a href="#" className={"relative pt-2 pb-3 flex-grow text-center rounded-tr-lg rounded-tl-lg mr-1 text-black no-underline hover:text-black hover:underline flex justify-center items-center"
        + (tab == 'product-list'? ` ${productsBg} shadow-inner-top` : '')} onClick={e => { e.preventDefault(); e.stopPropagation(); setTab('product-list'); }}>
          {/* <div className="bg-no-repeat w-6 h-6 mr-2 bg-img-product desaturate"></div> */}
          <span>Products</span>
      </a>
      <a href="#" className={"relative pt-2 pb-3 flex-grow text-center rounded-tl-lg text-black no-underline hover:text-black hover:underline flex justify-center items-center"
        + (tab == 'product-codes'? ` ${productCodesBg} shadow-inner-top` : '')} onClick={e => { e.preventDefault(); e.stopPropagation(); setTab('product-codes'); }}>
          {/* <div className="bg-no-repeat w-6 h-6 mr-2 bg-img-product desaturate"></div> */}
          <span>Codes</span>
      </a>
    </div>