package org.skyve.bizport;

/**
 * Uniquely defines a sheet with the Excel workbook.
 * There are 2 types of sheets - document sheets and collection sheets.
 * Document sheets have a key of module name and document name.
 * Collection sheets have a key of owning module name, owning document name and collection attribute name.
 */
public final class SheetKey implements Comparable<SheetKey> {
	private String moduleName;
	private String documentName;
	private String collectionBinding;

	/**
	 * 
	 * @param moduleName
	 * @param documentName
	 */
	public SheetKey(String moduleName, String documentName) {
		this.moduleName = moduleName;
		this.documentName = documentName;
	}
	
	/**
	 * 
	 * @param drivingModuleName
	 * @param drivingDocumentName
	 * @param collectionBinding
	 */
	public SheetKey(String drivingModuleName, String drivingDocumentName, String collectionBinding) {
		this.moduleName = drivingModuleName;
		this.documentName = drivingDocumentName;
		this.collectionBinding = collectionBinding;
	}

	/**
	 * 
	 * @return
	 */
	public String getModuleName() {
		return moduleName;
	}

	/**
	 * 
	 * @return
	 */
	public String getDocumentName() {
		return documentName;
	}

	/**
	 * 
	 * @return
	 */
	public String getCollectionBinding() {
		return collectionBinding;
	}
	
	/**
	 * Compare based on module name, document name, and optionally (for collection sheets)
	 * the collection attribute name.
	 */
	@Override
	public int compareTo(SheetKey o) {
		if (o == null) {
			return 1;
		}

		int result = moduleName.compareTo(o.moduleName);
		if (result == 0) { // equal
			result = documentName.compareTo(o.documentName);
			if (result == 0) { // equal
				if (collectionBinding == null) {
					if (o.collectionBinding != null) {
						result = -1;
					}
				}
				else {
					if (o.collectionBinding == null) {
						result = 1;
					}
					else {
						result = collectionBinding.compareTo(o.collectionBinding);
					}
				}
			}
		}
		
		return result;
	}

	/**
	 * 
	 */
	@Override
	public boolean equals(Object o) {
		if (o instanceof SheetKey) {
			return (compareTo((SheetKey) o) == 0);
		}
		return false;
	}

	/**
	 * 
	 */
	@Override
	public int hashCode() {
		return moduleName.hashCode() + 
				(17 * documentName.hashCode()) + 
				(23 * ((collectionBinding == null) ? 0 : collectionBinding.hashCode()));
	}

	/**
	 * 
	 */
	@Override
	public String toString() {
		StringBuilder result = new StringBuilder(64);
		result.append(moduleName).append('.').append(documentName);
		if (collectionBinding != null) {
			result.append(collectionBinding);
		}
		return result.toString();
	}
}
