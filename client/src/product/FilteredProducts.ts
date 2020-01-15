import { ProductCatalogueEntry } from 'util/Types'

const emptyFlags = {'b': false, 'g': false, 'o': false, 'f': false, 'v': false, 's': false}
export interface ProductFlags {[key: string]: boolean }

export class FilteredProducts {
  allProducts: ProductCatalogueEntry[]
  products: ProductCatalogueEntry[]
  searchString: string
  flags: ProductFlags
  category: string | null
  brand: string | null
  allCategories: string[]
  allBrands: string[]
  categories: string[]
  brands: string[]
  productFilter?: (p: ProductCatalogueEntry) => boolean

  constructor(allProducts: ProductCatalogueEntry[], allCategories: string[], allBrands: string[], products?: ProductCatalogueEntry[], searchString: string = '', flags: ProductFlags = emptyFlags, category: string | null = null, brand: string | null = null, productFilter?: (p: ProductCatalogueEntry) => boolean, categories?: string[], brands?: string[]) {
    this.allProducts = allProducts;
    this.allCategories = allCategories;
    this.allBrands = allBrands;
    this.products = products || allProducts;
    this.searchString = searchString;
    this.flags = flags;
    this.category = category;
    this.brand = brand;
    this.productFilter = productFilter
    this.categories = categories || allCategories
    this.brands = brands || allBrands
  }

  search = (searchString: string) => {
    return this.applyFilters(searchString.toLowerCase(), this.flags, this.category, this.brand, this.productFilter);
  }

  toggleFlag = (changedFlag: string) => {
    let flags:ProductFlags = {}
    for(let f in this.flags) {
      flags[f] = f == changedFlag? !this.flags[f] : this.flags[f]
    }

    return this.applyFilters(this.searchString, flags, this.category, this.brand, this.productFilter);
  }

  byCategory = (category: string | null) => {
    return this.applyFilters(this.searchString, this.flags, category, this.brand, this.productFilter);
  }

  byBrand = (brand: string | null) => {
    return this.applyFilters(this.searchString, this.flags, this.category, brand, this.productFilter);
  }

  filter = (productFilter: (p: ProductCatalogueEntry) => boolean) => {
    return this.applyFilters(this.searchString, this.flags, this.category, this.brand, productFilter);
  }

  private applyFilters = (searchString: string, flags: ProductFlags, category: string | null, brand: string | null, productFilter: ((p: ProductCatalogueEntry) => boolean) | undefined) => {
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

    if(brand) {
      filteredProducts = filteredProducts.filter(p => p.brand === brand)
    }

    if(productFilter) {
      filteredProducts = filteredProducts.filter(productFilter)
    }

    let brandProducts = (category? this.allProducts : filteredProducts).filter(p => !brand || p.brand === brand);
    let categories = brandProducts.map(p => p.category).reduce((cs, c) => cs.indexOf(c) === -1? [...cs, c] : cs, <string[]>[]).sort();
    let categoryProducts = (brand? this.allProducts : filteredProducts).filter(p => !category || p.category === category);
    let brands = categoryProducts.map(p => p.brand).reduce((bs, b) => bs.indexOf(b) === -1? [...bs, b] : bs, <string[]>[]).sort() ;

    return new FilteredProducts(this.allProducts, this.allCategories, this.allBrands, filteredProducts, searchString, flags, category, brand, productFilter, categories, brands)
  }
}