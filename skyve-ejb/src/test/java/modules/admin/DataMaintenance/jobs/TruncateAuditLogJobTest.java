package modules.admin.DataMaintenance.jobs;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.time.LocalDateTime;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.skyve.CORE;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.repository.customer.InterceptorMetaDataImpl;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.RDBMSAuditInterceptor;
import modules.admin.DataMaintenance.TruncateAuditLogJob;
import modules.admin.domain.Audit;
import modules.admin.domain.Audit.Operation;
import modules.admin.domain.Contact;
import modules.admin.domain.DataMaintenance;
import util.AbstractH2Test;

public class TruncateAuditLogJobTest extends AbstractH2Test {

	private DataMaintenance bean;

	@Before
	public void setup() throws Exception {
		bean = DataMaintenance.newInstance();
		DateTime epoch = new DateTime(LocalDateTime.of(2010, 1, 1, 0, 1));
		bean.setEpochDate(epoch);
		bean.setAuditLogRetentionDays(Integer.valueOf(0));
		bean = CORE.getPersistence().save(bean);
	}

	@Test
	public void testTruncateAuditLogJob() throws Exception {
		// setup test data
		Persistence pers = CORE.getPersistence();
		User user = pers.getUser();
		CustomerImpl customer = (CustomerImpl) user.getCustomer();

		// create and add the interceptor to the customer to generate Audits
		InterceptorMetaDataImpl interceptor = new InterceptorMetaDataImpl();
		interceptor.setClassName(RDBMSAuditInterceptor.class.getName());
		customer.putInterceptor(interceptor);

		// need to begin transaction to avoid H2 locking
		pers.begin();

		DataBuilder db = new DataBuilder();

		// first Audit bizId - check only the last two Update Audits exist
		Contact testContact = db.fixture(FixtureType.crud).build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		testContact = pers.save(testContact);

		String bizId = testContact.getBizId();
		testContact = db.fixture(FixtureType.crud).build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		testContact.setBizId(bizId);
		testContact = pers.save(testContact);

		bizId = testContact.getBizId();
		testContact = db.fixture(FixtureType.crud).build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		testContact.setBizId(bizId);
		testContact = pers.save(testContact);

		bizId = testContact.getBizId();
		testContact = db.fixture(FixtureType.crud).build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		testContact.setBizId(bizId);
		testContact = pers.save(testContact);

		// second Audit bizId - check Insert and Update Audits exist
		Contact testContact2 = db.fixture(FixtureType.crud).build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		testContact2 = pers.save(testContact2);

		bizId = testContact2.getBizId();
		testContact2 = db.fixture(FixtureType.crud).build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		testContact2.setBizId(bizId);
		testContact2 = pers.save(testContact2);

		// third Audit bizId - check Reconstruction Audits are created, then deleted after job runs
		Contact testContact3 = db.fixture(FixtureType.crud).build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		testContact3 = pers.save(testContact3);

		bizId = testContact3.getBizId();
		testContact3 = db.fixture(FixtureType.crud).build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		testContact3.setBizId(bizId);
		testContact3 = pers.save(testContact3);

		// commit to avoid H2 locking
		pers.commit(false);

		// need to begin transaction to avoid H2 locking
		pers.begin();
		// delete Audits of testContact3
		DocumentQuery q = pers.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
		q.getFilter().addEquals(Audit.auditBizIdPropertyName, testContact3.getBizId());
		List<Audit> audits = q.beanResults();
		for (Audit audit : audits) {
			pers.delete(audit);
		}

		// commit to avoid H2 locking
		pers.commit(false);
		
		// begin transaction to avoid H2 locking
		pers.begin();
		// update testContact3 so that Reconstruction Audit is made
		bizId = testContact3.getBizId();
		testContact3 = db.fixture(FixtureType.crud).build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		testContact3.setBizId(bizId);
		testContact3 = pers.save(testContact3);

		// commit to avoid H2 locking
		pers.commit(false);
		
		q.getFilter().addEquals(Audit.operationPropertyName, Operation.reconstruction);
		Audit result = q.beanResult();

		// check that Reconstruction audit exists for testContact3
		assertNotNull(result);

		// Avoid calling an asynchronous job and execute the job from the class
		TruncateAuditLogJob j = new TruncateAuditLogJob();

		// begin transaction to avoid H2 locking
		pers.begin();

		// call the method under test
		j.truncate(bean, pers);

		// commit to avoid H2 locking
		pers.commit(false);

		// verify the result
		// testContact1 should have 2 update Audits
		DocumentQuery q1 = pers.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
		q1.getFilter().addEquals(Audit.auditBizIdPropertyName, testContact.getBizId());
		q1.getFilter().addEquals(Audit.operationPropertyName, Operation.update);
		List<Audit> q1Audits = q1.beanResults();

		assertEquals(q1Audits.size(), 2);

		// testContact2 should have 2 Audits
		DocumentQuery q2 = pers.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
		q2.getFilter().addEquals(Audit.auditBizIdPropertyName, testContact2.getBizId());
		List<Audit> q2Audits = q2.beanResults();
		
		assertEquals(q2Audits.size(), 2);

		// testContact3 should have no reconstruction Audits
		DocumentQuery q3 = pers.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
		q3.getFilter().addEquals(Audit.auditBizIdPropertyName, testContact3.getBizId());
		q3.getFilter().addEquals(Audit.operationPropertyName, Operation.reconstruction);
		List<Audit> q3Audits = q3.beanResults();

		assertEquals(q3Audits.size(), 0);
	}
}
