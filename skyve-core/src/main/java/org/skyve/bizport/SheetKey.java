package org.skyve.bizport;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Uniquely defines a sheet with the Excel workbook.
 * There are 2 types of sheets - document sheets and collection sheets.
 * Document sheets have a key of module name and document name.
 * Collection sheets have a key of owning module name, owning document name and collection attribute name.
 */
public final class SheetKey implements Comparable<SheetKey> {
	@Nonnull private String moduleName;
	@Nonnull private String documentName;
	@Nullable private String collectionBinding;

	/**
	 * 
	 * @param moduleName
	 * @param documentName
	 */
	public SheetKey(@Nonnull String moduleName, @Nonnull String documentName) {
		this.moduleName = moduleName;
		this.documentName = documentName;
	}
	
	/**
	 * 
	 * @param drivingModuleName
	 * @param drivingDocumentName
	 * @param collectionBinding
	 */
	public SheetKey(@Nonnull String drivingModuleName, @Nonnull String drivingDocumentName, @Nonnull String collectionBinding) {
		this.moduleName = drivingModuleName;
		this.documentName = drivingDocumentName;
		this.collectionBinding = collectionBinding;
	}

	/**
	 * 
	 * @return
	 */
	public @Nonnull String getModuleName() {
		return moduleName;
	}

	/**
	 * 
	 * @return
	 */
	public @Nonnull String getDocumentName() {
		return documentName;
	}

	/**
	 * 
	 * @return
	 */
	public @Nullable String getCollectionBinding() {
		return collectionBinding;
	}
	
	/**
	 * Compare based on module name, document name, and optionally (for collection sheets)
	 * the collection attribute name.
	 */
	@Override
	@SuppressWarnings("null") // collection binding null checks are correct below
	public int compareTo(@Nullable SheetKey o) {
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
	public boolean equals(@Nullable Object o) {
		if (o instanceof SheetKey sheetKey) {
			return (compareTo(sheetKey) == 0);
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
	public @Nonnull String toString() {
		StringBuilder result = new StringBuilder(64);
		result.append(moduleName).append('.').append(documentName);
		if (collectionBinding != null) {
			result.append(collectionBinding);
		}
		return result.toString();
	}
}
