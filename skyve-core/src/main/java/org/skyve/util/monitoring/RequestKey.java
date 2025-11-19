package org.skyve.util.monitoring;

import org.skyve.impl.metadata.view.model.chart.ChartBuilderMetaData;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;

import jakarta.annotation.Nonnull;

public class RequestKey {
	private char type;
	private String moduleName;
	private String documentName;
	private String component;
	
	public static final RequestKey NONE = new RequestKey(' ', null, null, null);

	private RequestKey(char type, String moduleName, String documentName, String component) {
		this.type = type;
		this.moduleName = moduleName;
		this.documentName = documentName;
		this.component = component;
	}

	public static RequestKey chart(ChartBuilderMetaData model) {
		String queryName = model.getQueryName();
		String modelName = model.getModelName();
		return new RequestKey('M', model.getModuleName(), model.getDocumentName(), (queryName == null) ? modelName : queryName);
	}

	public static RequestKey create(Document document) {
		return new RequestKey('C', document.getOwningModuleName(), document.getName(), null);
	}

	public static RequestKey edit(Document document) {
		return new RequestKey('E', document.getOwningModuleName(), document.getName(), null);
	}

	public static RequestKey save(Document document) {
		return new RequestKey('S', document.getOwningModuleName(), document.getName(), null);
	}
	
	public static RequestKey delete(Document document) {
		return new RequestKey('D', document.getOwningModuleName(), document.getName(), null);
	}
	
	public static RequestKey zoomOut(Document document) {
		return new RequestKey('Z', document.getOwningModuleName(), document.getName(), null);
	}

	public static RequestKey rerender(Document document) {
		return new RequestKey('R', document.getOwningModuleName(), document.getName(), null);
	}

	public static RequestKey action(Document document, String actionName) {
		return new RequestKey('A', document.getOwningModuleName(), document.getName(), actionName);
	}
	
	public static RequestKey documentListModel(String moduleName, String documentName) {
		return new RequestKey('M', moduleName, documentName, null);
	}

	public static RequestKey queryListModel(String moduleName, String queryName) {
		return new RequestKey('M', moduleName, null, queryName);
	}

	public static RequestKey model(Document document, String modelName) {
		return new RequestKey('M', document.getOwningModuleName(), document.getName(), modelName);
	}

	public static RequestKey complete(Document document, String attributeName) {
		return new RequestKey('O', document.getOwningModuleName(), document.getName(), attributeName);
	}

	public static RequestKey dynamicImage(Document document, String imageName) {
		return new RequestKey('I', document.getOwningModuleName(), document.getName(), imageName);
	}

	@Override
	public String toString() {
		if (this == NONE) {
			return null;
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

	public static @Nonnull RequestKey fromString(@Nonnull String keyCode) {
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

	public @Nonnull DomainValue toDomainValue() {
		return new DomainValue(toString());
	}

	public char getType() {
		return type;
	}

	public String getModuleName() {
		return moduleName;
	}

	public String getDocumentName() {
		return documentName;
	}

	public String getComponent() {
		return component;
	}
	
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
