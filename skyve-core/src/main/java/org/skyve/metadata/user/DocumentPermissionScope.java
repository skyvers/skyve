package org.skyve.metadata.user;

/**
 * Defines the data visibility scope applied to a {@link DocumentPermission}.
 *
 * <p>The scope limits which persisted instances of a document a user can see or modify.
 * Scopes form a strict hierarchy from most restrictive ({@link #none}) to least
 * restrictive ({@link #global}):
 * <ol>
 *   <li>{@link #none} &mdash; no instances are accessible.</li>
 *   <li>{@link #user} &mdash; only instances owned by this user.</li>
 *   <li>{@link #dataGroup} &mdash; all instances within the user's data group.</li>
 *   <li>{@link #customer} &mdash; all instances for the current customer (tenant).</li>
 *   <li>{@link #global} &mdash; all instances across every customer/tenant.</li>
 * </ol>
 *
 * <p>When two permissions are merged via
 * {@link DocumentPermission#mergePermission}, the broader (higher-ordinal) scope wins.
 *
 * @see DocumentPermission
 */
@SuppressWarnings("java:S115") // Suppress "Constant names should comply with a naming convention" as these are not constants but enum values
public enum DocumentPermissionScope {
	/** No instances are accessible regardless of ownership. */
	none('_'),
	
	/** Only instances directly owned by the current user are accessible. */
	user('U'),
	
	/** All instances belonging to the current user's data group are accessible. */
	dataGroup('D'),
	
	/** All instances belonging to the current customer (tenant) are accessible. */
	customer('C'),
	
	/** All instances across every customer and tenant are accessible (super-user scope). */
	global('G');

	private char code;

	private DocumentPermissionScope(char code) {
		this.code = code;
	}

	/**
	 * Returns the single-character code identifying this scope in a
	 * {@link DocumentPermission} enum constant name.
	 *
	 * @return the scope character ({@code '_'}, {@code 'U'}, {@code 'D'},
	 *         {@code 'C'}, or {@code 'G'})
	 */
	public char charValue() {
		return code;
	}
}
