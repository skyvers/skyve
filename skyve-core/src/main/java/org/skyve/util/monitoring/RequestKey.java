package org.skyve.util.monitoring;

import org.skyve.impl.metadata.view.model.chart.ChartBuilderMetaData;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;

import jakarta.annotation.Nonnull;

/**
 * Encodes a monitorable request operation into a compact key string.
 *
 * <p>Keys are used as map identifiers inside {@link org.skyve.util.monitoring.Monitoring}
 * and preserve operation type, module, optional document, and optional component.
 * Factory methods create keys for common operation categories.
 *
 * <p>Wire format produced by {@link #toString()} is
 * {@code {type}{module}[.{document}][^{component}]}. This format is parsed by
 * {@link #fromString(String)}.
 *
 * <p>Threading: instances are immutable after construction and are safe to share.
 */
public class RequestKey {
	private char type;
	private String moduleName;
	private String documentName;
	private String component;
	
	/**
	 * Sentinel key used where no request classification is available.
	 */
	public static final RequestKey NONE = new RequestKey(' ', null, null, null);

	private RequestKey(char type, String moduleName, String documentName, String component) {
		this.type = type;
		this.moduleName = moduleName;
		this.documentName = documentName;
		this.component = component;
	}

	/**
	 * Creates a model-request key for chart model execution.
	 *
	 * @param model	chart model metadata
	 * @return key encoded as model request type ({@code M})
	 */
	public static RequestKey chart(ChartBuilderMetaData model) {
		String queryName = model.getQueryName();
		String modelName = model.getModelName();
		return new RequestKey('M', model.getModuleName(), model.getDocumentName(), (queryName == null) ? modelName : queryName);
	}

	/**
	 * Creates a request key for create operations.
	 *
	 * @param document	document being created
	 * @return key encoded as create request type ({@code C})
	 */
	public static RequestKey create(Document document) {
		return new RequestKey('C', document.getOwningModuleName(), document.getName(), null);
	}

	/**
	 * Creates a request key for edit operations.
	 *
	 * @param document	document being edited
	 * @return key encoded as edit request type ({@code E})
	 */
	public static RequestKey edit(Document document) {
		return new RequestKey('E', document.getOwningModuleName(), document.getName(), null);
	}

	/**
	 * Creates a request key for save operations.
	 *
	 * @param document	document being saved
	 * @return key encoded as save request type ({@code S})
	 */
	public static RequestKey save(Document document) {
		return new RequestKey('S', document.getOwningModuleName(), document.getName(), null);
	}
	
	/**
	 * Creates a request key for delete operations.
	 *
	 * @param document	document being deleted
	 * @return key encoded as delete request type ({@code D})
	 */
	public static RequestKey delete(Document document) {
		return new RequestKey('D', document.getOwningModuleName(), document.getName(), null);
	}
	
	/**
	 * Creates a request key for zoom-out navigation operations.
	 *
	 * @param document	current document context
	 * @return key encoded as zoom-out request type ({@code Z})
	 */
	public static RequestKey zoomOut(Document document) {
		return new RequestKey('Z', document.getOwningModuleName(), document.getName(), null);
	}

	/**
	 * Creates a request key for rerender operations.
	 *
	 * @param document	current document context
	 * @return key encoded as rerender request type ({@code R})
	 */
	public static RequestKey rerender(Document document) {
		return new RequestKey('R', document.getOwningModuleName(), document.getName(), null);
	}

	/**
	 * Creates a request key for server-side action execution.
	 *
	 * @param document	action host document
	 * @param actionName	action name
	 * @return key encoded as action request type ({@code A})
	 */
	public static RequestKey action(Document document, String actionName) {
		return new RequestKey('A', document.getOwningModuleName(), document.getName(), actionName);
	}
	
	/**
	 * Creates a request key for document list-model retrieval.
	 *
	 * @param moduleName	module name
	 * @param documentName	document name
	 * @return key encoded as model request type ({@code M})
	 */
	public static RequestKey documentListModel(String moduleName, String documentName) {
		return new RequestKey('M', moduleName, documentName, null);
	}

	/**
	 * Creates a request key for query list-model retrieval.
	 *
	 * @param moduleName	module name
	 * @param queryName	query name
	 * @return key encoded as model request type ({@code M})
	 */
	public static RequestKey queryListModel(String moduleName, String queryName) {
		return new RequestKey('M', moduleName, null, queryName);
	}

	/**
	 * Creates a request key for custom model execution.
	 *
	 * @param document	model host document
	 * @param modelName	model name
	 * @return key encoded as model request type ({@code M})
	 */
	public static RequestKey model(Document document, String modelName) {
		return new RequestKey('M', document.getOwningModuleName(), document.getName(), modelName);
	}

	/**
	 * Creates a request key for autocomplete completion operations.
	 *
	 * @param document	document containing the completed attribute
	 * @param attributeName	attribute name being completed
	 * @return key encoded as complete request type ({@code O})
	 */
	public static RequestKey complete(Document document, String attributeName) {
		return new RequestKey('O', document.getOwningModuleName(), document.getName(), attributeName);
	}

	/**
	 * Creates a request key for dynamic-image rendering operations.
	 *
	 * @param document	document context for the image
	 * @param imageName	dynamic image name
	 * @return key encoded as dynamic-image request type ({@code I})
	 */
	public static RequestKey dynamicImage(Document document, String imageName) {
		return new RequestKey('I', document.getOwningModuleName(), document.getName(), imageName);
	}

	/**
	 * Serialises this key to the compact wire format used by monitoring maps.
	 *
	 * @return serialised key string, or empty string for {@link #NONE}
	 */
	@Override
	public String toString() {
		if (this == NONE) {
			return "";
		}

		StringBuilder result = new StringBuilder(128);
		result.append(type).append(moduleName);
		if (documentName != null) {
			result.append('.').append(documentName);
		}
		if (component != null) {
			result.append('^').append(component);
		}
		return result.toString();
	}

	/**
	 * Parses a key produced by {@link #toString()}.
	 *
	 * @param keyCode	serialised key string in compact wire format
	 * @return reconstructed request key
	 */
	public static @Nonnull RequestKey fromString(@Nonnull String keyCode) {
		if (keyCode.isEmpty()) {
			return NONE;
		}

		char type = keyCode.charAt(0);
		String remaining = keyCode.substring(1);

		int dotIndex = remaining.indexOf('.');
		int caretIndex = remaining.indexOf('^');

		String module = null;
		String document = null;
		String component = null;

		// Case 1: Has caret but no dot - format: {type}{module}^{component} or {type}^{component}
		if (caretIndex >= 0 && dotIndex < 0) {
			if (caretIndex > 0) {
				module = remaining.substring(0, caretIndex);
			}
			component = remaining.substring(caretIndex + 1);
		}
		// Case 2: Has dot but no caret - format: {type}{module}.{document}
		else if (dotIndex >= 0 && caretIndex < 0) {
			module = remaining.substring(0, dotIndex);
			document = remaining.substring(dotIndex + 1);
		}
		// Case 3: Has both dot and caret - format: {type}{module}.{document}^{component}
		else if (dotIndex >= 0 && caretIndex >= 0) {
			// Determine which comes first
			if (dotIndex < caretIndex) {
				// Normal case: module.document^component
				module = remaining.substring(0, dotIndex);
				document = remaining.substring(dotIndex + 1, caretIndex);
				component = remaining.substring(caretIndex + 1);
			}
			else {
				// Edge case: module^component.with.dots
				module = remaining.substring(0, caretIndex);
				component = remaining.substring(caretIndex + 1);
			}
		}
		// Case 4: No dot and no caret - format: {type}{module}
		else {
			if (! remaining.isEmpty()) {
				module = remaining;
			}
		}

		return new RequestKey(type, module, document, component);
	}

	/**
	 * Converts this key to a domain value suitable for UI list models.
	 *
	 * @return domain value carrying the serialised key code
	 */
	public @Nonnull DomainValue toDomainValue() {
		return new DomainValue(toString());
	}

	/**
	 * Returns the operation type code.
	 */
	public char getType() {
		return type;
	}

	/**
	 * Returns the module component of the key.
	 */
	public String getModuleName() {
		return moduleName;
	}

	/**
	 * Returns the document component of the key, when present.
	 */
	public String getDocumentName() {
		return documentName;
	}

	/**
	 * Returns the component suffix (action/query/model/attribute/image), when present.
	 */
	public String getComponent() {
		return component;
	}
	
	/**
	 * Returns a human-readable description for diagnostics and admin tooling.
	 *
	 * @return descriptive label including type, module, and optional document/component
	 */
	public String toDescription() {
		StringBuilder result = new StringBuilder(128);
	
		switch (type) {
			case 'M':
				result.append("Model ");
				break;
			case 'C':
				result.append("Create ");
				break;
			case 'E':
				result.append("Edit ");
				break;
			case 'S':
				result.append("Save ");
				break;
			case 'D':
				result.append("Delete ");
				break;
			case 'Z':
				result.append("Zoom Out ");
				break;
			case 'R':
				result.append("Rerender ");
				break;
			case 'A':
				result.append("Action ");
				break;
			case 'O':
				result.append("Complete ");
				break;
			case 'I':
				result.append("Dynamic Image ");
				break;
			default:
				result.append("Unknown ");
		}
		result.append(moduleName);
		if (documentName != null) {
			result.append('.').append(documentName);
		}
		if (component != null) {
			result.append(' ').append(component);
		}
		return result.toString();
	}
}
