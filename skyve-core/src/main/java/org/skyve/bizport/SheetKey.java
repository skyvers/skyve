package org.skyve.bizport;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Uniquely defines a sheet with the Excel workbook.
 * There are 2 types of sheets - document sheets and collection sheets.
 * Document sheets have a key of module name and document name.
 * Collection sheets have a key of owning module name, owning document name and collection attribute name.
 *
 * <p>Ordering and equality are value-based across all key components and are used for
 * map/set lookups when wiring cross-sheet references in {@link BizPortWorkbook}.
 */
public final class SheetKey implements Comparable<SheetKey> {
	@Nonnull private String moduleName;
	@Nonnull private String documentName;
	@Nullable private String collectionBinding;

	/**
	 * Creates a key for a document sheet.
	 *
	 * @param moduleName the owning module name; must not be {@code null}
	 * @param documentName the document name within the module; must not be {@code null}
	 */
	public SheetKey(@Nonnull String moduleName, @Nonnull String documentName) {
		this.moduleName = moduleName;
		this.documentName = documentName;
	}
	
	/**
	 * Creates a key for a collection sheet.
	 *
	 * @param drivingModuleName the module containing the owning document; must not be {@code null}
	 * @param drivingDocumentName the owning document name; must not be {@code null}
	 * @param collectionBinding the collection attribute binding on the owning document; must not be {@code null}
	 */
	public SheetKey(@Nonnull String drivingModuleName, @Nonnull String drivingDocumentName, @Nonnull String collectionBinding) {
		this.moduleName = drivingModuleName;
		this.documentName = drivingDocumentName;
		this.collectionBinding = collectionBinding;
	}

	/**
	 * Returns the module component of this key.
	 *
	 * @return the module name; never {@code null}
	 */
	public @Nonnull String getModuleName() {
		return moduleName;
	}

	/**
	 * Returns the document component of this key.
	 *
	 * @return the document name; never {@code null}
	 */
	public @Nonnull String getDocumentName() {
		return documentName;
	}

	/**
	 * Returns the collection-binding component for collection sheets.
	 *
	 * @return the collection binding, or {@code null} for document sheets
	 */
	public @Nullable String getCollectionBinding() {
		return collectionBinding;
	}
	
	/**
	 * Compare based on module name, document name, and optionally (for collection sheets)
	 * the collection attribute name.
	 *
	 * @param o the key to compare against
	 * @return a negative value, zero, or a positive value according to lexicographic order
	 *         of module, document, and collection binding
	 */
	@Override
	@SuppressWarnings({"null", "java:S3776"}) // collection binding null checks are correct below; Complexity OK
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
	 * Returns whether this key is value-equal to another object.
	 *
	 * @param o the candidate object
	 * @return {@code true} when {@code o} is a {@link SheetKey} with equal module,
	 *         document, and collection-binding components
	 */
	@Override
	public boolean equals(@Nullable Object o) {
		if (o instanceof SheetKey sheetKey) {
			return (compareTo(sheetKey) == 0);
		}
		return false;
	}

	/**
	 * Returns a hash code derived from all key components.
	 *
	 * @return a stable value-based hash code
	 */
	@Override
	public int hashCode() {
		return moduleName.hashCode() + 
				(17 * documentName.hashCode()) + 
				(23 * ((collectionBinding == null) ? 0 : collectionBinding.hashCode()));
	}

	/**
	 * Returns a compact textual form of this key.
	 *
	 * @return a string containing module, document, and optional collection components
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
