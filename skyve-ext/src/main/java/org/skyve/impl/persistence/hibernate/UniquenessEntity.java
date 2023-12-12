package org.skyve.impl.persistence.hibernate;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;

/**
 * This class is only used to generate the ADM_Uniqueness table
 */
@Entity(name = "ADM_Uniqueness")
public class UniquenessEntity {
	private UniquenessEntity() {
		// prevent instantiation
	}

	@Id
	@Column(name = "hash", length = 64)
	private String hash;
}
