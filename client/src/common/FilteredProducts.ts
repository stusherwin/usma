import { ProductCatalogueEntry } from '../Types'

const emptyFlags = {'b': false, 'g': false, 'o': false, 'f': false, 'v': false, 's': false}
export interface ProductFlags {[key: string]: boolean }

export class FilteredProducts {
  allProducts: ProductCatalogueEntry[]
  products: ProductCatalogueEntry[]
  searchString: string
  flags: ProductFlags
  category: string | null
  allCategories: string[]
  productFilter?: (p: ProductCatalogueEntry) => boolean

  constructor(allProducts: ProductCatalogueEntry[], allCategories: string[], products?: ProductCatalogueEntry[], searchString: string = '', flags: ProductFlags = emptyFlags, category: string | null = null, productFilter?: (p: ProductCatalogueEntry) => boolean) {
    this.allProducts = allProducts;
    this.allCategories = allCategories;
    this.products = products || allProducts;
    this.searchString = searchString;
    this.flags = flags;
    this.category = category;
    this.productFilter = productFilter
  }

  search = (searchString: string) => {
    return this.applyFilters(searchString.toLowerCase(), this.flags, this.category, this.productFilter);
  }

  toggleFlag = (changedFlag: string) => {
    let flags:ProductFlags = {}
    for(let f in this.flags) {
      flags[f] = f == changedFlag? !this.flags[f] : this.flags[f]
    }

    return this.applyFilters(this.searchString, flags, this.category, this.productFilter);
  }

  byCategory = (category: string | null) => {
    return this.applyFilters(this.searchString, this.flags, category, this.productFilter);
  }

  filter = (productFilter: (p: ProductCatalogueEntry) => boolean) => {
    return this.applyFilters(this.searchString, this.flags, this.category, productFilter)
  }

  private applyFilters = (searchString: string, flags: ProductFlags, category: string | null, productFilter?: (p: ProductCatalogueEntry) => boolean) => {
    var filteredProducts = this.allProducts

    if(searchString.length) {
      filteredProducts = filteredProducts.filter((p: ProductCatalogueEntry) => {
        const code = p.code.toLowerCase()
        const name = p.name.toLowerCase()
        const searchWords = searchString.split(' ')
        
        return searchWords.every(w => code.includes(w) || name.includes(w))
      })
    }
    
    if(flags['b'] || flags['g'] || flags['o'] || flags['f'] || flags['v'] || flags['s']) {
      filteredProducts = filteredProducts.filter((p: ProductCatalogueEntry) => 
               (!flags['b'] || p.biodynamic)
            && (!flags['g'] || p.glutenFree)
            && (!flags['o'] || p.organic)
            && (!flags['f'] || p.fairTrade)
            && (!flags['v'] || p.vegan)
            && (!flags['s'] || p.addedSugar)
      )
    }

    if(category) {
      filteredProducts = filteredProducts.filter(p => p.category === category)
    }

    if(productFilter) {
      filteredProducts = filteredProducts.filter(productFilter)
    }

    return new FilteredProducts(this.allProducts, this.allCategories, filteredProducts, searchString, flags, category, productFilter)
  }
}