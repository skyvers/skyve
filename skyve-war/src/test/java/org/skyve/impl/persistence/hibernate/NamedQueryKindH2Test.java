package org.skyve.impl.persistence.hibernate;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.metadata.MetaDataException;
import org.skyve.persistence.Persistence;

import modules.admin.domain.Contact;
import util.AbstractH2Test;

/**
 * Tests that the persistence's named BizQL and SQL queries are looked up by kind: a metadata query's
 * name, which the admin module's qContacts is, and a name the module does not declare, are no BizQL
 * or SQL query's, and each throws a MetaDataException naming the query and the module.
 */
@SuppressWarnings("static-method")
class NamedQueryKindH2Test extends AbstractH2Test {
	@Test
	void aNamedBizQLOrSQLOfAnotherKindOrMissingThrows() {
		Persistence p = CORE.getPersistence();

		assertAll(() -> assertEquals("BizQL query qContacts does not exist in module admin",
										assertThrows(MetaDataException.class, () -> p.newNamedBizQL(Contact.MODULE_NAME, "qContacts")).getMessage()),
					() -> assertEquals("BizQL query qMissing does not exist in module admin",
										assertThrows(MetaDataException.class, () -> p.newNamedBizQL(Contact.MODULE_NAME, "qMissing")).getMessage()),
					() -> assertEquals("SQL query qContacts does not exist in module admin",
										assertThrows(MetaDataException.class, () -> p.newNamedSQL(Contact.MODULE_NAME, "qContacts")).getMessage()),
					() -> assertEquals("SQL query qMissing does not exist in module admin",
										assertThrows(MetaDataException.class, () -> p.newNamedSQL(Contact.MODULE_NAME, "qMissing")).getMessage()));
	}
}
