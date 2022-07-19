package org.skyve.metadata.user;

public class UserAccess {
	private char type;
	private String moduleName;
	private String documentName;
	private String queryName;
	
	private UserAccess(char type, String moduleName, String documentName, String queryName) {
		this.type = type;
		this.moduleName = moduleName;
		this.documentName = documentName;
		this.queryName = queryName;
	}

	public static UserAccess singular(String moduleName, String documentName) {
		return new UserAccess('S', moduleName, documentName, null);
	}
	
	public static UserAccess documentAggregate(String moduleName, String documentName) {
		return new UserAccess('A', moduleName, null, documentName);
	}

	public static UserAccess queryAggregate(String moduleName, String queryName) {
		return new UserAccess('A', moduleName, null, queryName);
	}

	public static UserAccess modelAggregate(String moduleName, String documentName, String modelName) {
		return new UserAccess('A', moduleName, documentName, modelName);
	}

	@Override
	public String toString() {
		StringBuilder result = new StringBuilder(64);
		result.append(type);
		result.append(moduleName);
		if (documentName != null) {
			result.append('.');
			result.append(documentName);
		}
		if (queryName != null) {
			result.append('.');
			result.append(queryName);
		}		
		return result.toString();
	}
}
