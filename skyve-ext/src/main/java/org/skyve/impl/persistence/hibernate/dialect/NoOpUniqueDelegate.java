package org.skyve.impl.persistence.hibernate.dialect;

import org.hibernate.boot.Metadata;
import org.hibernate.dialect.Dialect;
import org.hibernate.dialect.unique.DefaultUniqueDelegate;
import org.hibernate.mapping.UniqueKey;

/**
 * Suppresses Hibernate unique-key DDL generation for dialects that cannot use Skyve's desired semantics.
 */
public class NoOpUniqueDelegate extends DefaultUniqueDelegate {
	/**
	 * Creates the delegate for the supplied Hibernate dialect.
	 *
	 * @param dialect the owning Hibernate dialect
	 */
	public NoOpUniqueDelegate(Dialect dialect) {
		super(dialect);
	}
	
	/**
	 * Returns no SQL so schema migration skips unique-key creation.
	 *
	 * @param uniqueKey the unique key Hibernate would otherwise create
	 * @param metadata the mapping metadata
	 * @return an empty string
	 */
	@Override
	public String getAlterTableToAddUniqueKeyCommand(UniqueKey uniqueKey, Metadata metadata) {
		return "";
	}
	
	/**
	 * Returns no SQL so schema migration skips unique-key removal.
	 *
	 * @param uniqueKey the unique key Hibernate would otherwise drop
	 * @param metadata the mapping metadata
	 * @return an empty string
	 */
	@Override
	public String getAlterTableToDropUniqueKeyCommand(UniqueKey uniqueKey, Metadata metadata) {
		return "";
	}
}
