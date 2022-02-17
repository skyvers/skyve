package org.skyve.impl.domain.number;

import javax.persistence.EntityManager;

import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.metadata.user.User;

public class DocumentNumberAutonomousTransactionGenerator extends AbstractDocumentNumberGenerator {

	@Override
	public String next(String prefix, String moduleName, String documentName, String fieldName, int numberLength) throws Exception {
		String nextNumber = "0";

		AbstractHibernatePersistence pers = (AbstractHibernatePersistence) CORE.getPersistence();
		try {
			pers = pers.getClass().getConstructor().newInstance();
		} catch (Exception e) {
			throw new DomainException("Could not instantiate Persistence", e);
		}
		try {
			User user = CORE.getPersistence().getUser();
			pers.setUser(user);
			pers.begin();
			nextNumber = getNextNumber(pers, prefix, moduleName, documentName, fieldName, numberLength);
		} finally {
			EntityManager em = pers.getEntityManager();
			try {
				em.getTransaction().commit();
			} finally {
				// Can't call tempP.commit(true) here as it would remove the current thread's Persistence as well
				em.close();
			}
		}

		return nextNumber;
	}


}
