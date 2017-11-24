package org.skyve.metadata.user;

/**
 * 
 */
public enum DocumentPermissionScope {
	/**
	 * 
	 */
	none('_'),
	
	/**
	 * 
	 */
	user('U'),
	
	/**
	 * 
	 */
	dataGroup('D'),
	
	/**
	 * 
	 */
	customer('C'),
	
	/**
	 * 
	 */
	global('G');

	private char code;

	/**
	 * 
	 * @param code
	 */
	private DocumentPermissionScope(char code) {
		this.code = code;
	}

	/**
	 * 
	 * @return
	 */
	public char charValue() {
		return code;
	}
}
