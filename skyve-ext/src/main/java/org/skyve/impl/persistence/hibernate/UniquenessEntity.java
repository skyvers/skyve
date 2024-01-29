package org.skyve.impl.persistence.hibernate;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;

/**
 * This class is only used to generate the ADM_Uniqueness table
 */
@Entity(name = UniquenessEntity.TABLE_NAME)
public class UniquenessEntity {
	public static final String TABLE_NAME = "ADM_Uniqueness";
	public static final String HASH_COLUMN_NAME = "hash";

	private UniquenessEntity() {
		// prevent instantiation
	}

	@Id
	@Column(name = UniquenessEntity.HASH_COLUMN_NAME, length = 64)
	private String hash;
}
