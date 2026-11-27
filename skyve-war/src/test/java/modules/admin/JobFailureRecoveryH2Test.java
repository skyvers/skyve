package modules.admin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.Persistence;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.ControlPanel.GenerateTestDataJob;
import modules.admin.DataMaintenance.DataMaintenanceExtension;
import modules.admin.DataMaintenance.RefreshDocumentTuplesJob;
import modules.admin.Group.GroupExtension;
import modules.admin.Tag.DeleteAllTaggedDataForTagJob;
import modules.admin.Tag.PerformDocumentActionForTagJob;
import modules.admin.Tag.TagDefaultAction;
import modules.admin.Tag.TagExtension;
import modules.admin.Tag.TagService;
import modules.admin.User.UserExtension;
import modules.admin.domain.AdminDocument;
import modules.admin.domain.ControlPanel;
import modules.admin.domain.DataMaintenance;
import modules.admin.domain.DataMaintenance.EvictOption;
import modules.admin.domain.DataMaintenance.RefreshOption;
import modules.admin.domain.ModuleDocument;
import modules.admin.domain.Tag;
import modules.admin.domain.UserProxy;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.Hierarchical;
import modules.test.domain.UniqueConstraintNonNullable;
import util.AbstractH2TestTruncate;

/**
 * Jobs that commit each item and carry on after a failed one.
 * A failure in the database leaves the transaction rollback-only, so each job rolls back and begins again,
 * or the next item's commit would roll back with the failed one. Without that, the item after each failure was
 * silently lost (and on PostgreSQL every item after it failed). The rollback detaches the items still to come,
 * so the jobs retrieve them again.
 * <p>
 * The failures come from constraints added for the test only, which Skyve does not know about, so they fail
 * in the database: a check constraint that one row breaks, or a foreign key from a scratch table to one row.
 * The items are named so that the failing one is second, in bizId and bizKey order alike.
 * LoadDemonstrationDataJob is in the whosin module, which the test customer does not have, so it is tested with mocks
 * in LoadDemonstrationDataJobTest.
 */
@SuppressWarnings("static-method")
class JobFailureRecoveryH2Test extends AbstractH2TestTruncate {
	private static final String AAP_TABLE = "TEST_AllAttributesPersistent";
	private static final String UNIQUE_TABLE = "TEST_UniqueConstraintNonNullable";
	private static final String USER_TABLE = "ADM_SecurityUser";
	private static final String CONSTRAINT = "RBO_FAILURE";
	private static final String BLOCKER_TABLE = "RBO_BLOCKER";
	private static final String FAILING_TEXT = "2 fails";
	private static final String FAILING_ID = "rbo-2";
	private static final List<String> TEXTS = List.of("1 before", FAILING_TEXT, "3 after", "4 last");
	private static final String FLAG = "Data refresh failed - Please validate data and try again.";
	private static final String UNSUCCESSFUL = " - Unsuccessful";

	@Test
	void testRefreshDocumentTuplesRefreshesTheRestAndFlagsTheFailure() throws Exception {
		saveAaps();
		// The failing row can be written once it is flagged
		addConstraint(AAP_TABLE, "text <> '" + FAILING_TEXT + "' or bizFlagComment is not null");
		try {
			RefreshDocumentTuplesJob job = refreshJob(Boolean.FALSE);

			job.execute();
			commit();

			// Every upsert adds 1 to the version: the failed one's flag did, and the others' refresh did
			assertEquals(List.of("1 before|1|null", FAILING_TEXT + "|1|" + FLAG, "3 after|1|null", "4 last|1|null"), aapRows());
			assertTrue(job.getLog().stream().anyMatch(l -> l.endsWith("failed for id: " + FAILING_ID)), job.getLog().toString());
		}
		finally {
			dropConstraint(AAP_TABLE);
		}
	}

	@Test
	void testRefreshDocumentTuplesFinishesAndNotifiesAfterTheLastItemFails() throws Exception {
		saveAaps();
		addConstraint(AAP_TABLE, "text <> '4 last' or bizFlagComment is not null");
		try {
			RefreshDocumentTuplesJob job = refreshJob(Boolean.TRUE);

			job.execute();
			commit();

			assertEquals(List.of("1 before|1|null", FAILING_TEXT + "|1|null", "3 after|1|null", "4 last|1|" + FLAG), aapRows());
			List<String> log = job.getLog();
			assertTrue(log.get(log.size() - 1).startsWith("Finished Document Data Refresh Job"), log.toString());
		}
		finally {
			dropConstraint(AAP_TABLE);
		}
	}

	@Test
	void testRefreshDocumentTuplesLogsAFlagThatCannotBeWrittenAndCarriesOn() throws Exception {
		saveAaps();
		// The failing row cannot be written at all, so neither can its flag
		addConstraint(AAP_TABLE, "text <> '" + FAILING_TEXT + "'");
		try {
			RefreshDocumentTuplesJob job = refreshJob(Boolean.FALSE);

			job.execute();
			commit();

			assertEquals(List.of("1 before|1|null", FAILING_TEXT + "|0|null", "3 after|1|null", "4 last|1|null"), aapRows());
			assertTrue(job.getLog().stream().anyMatch(l -> l.startsWith("Could not flag id: " + FAILING_ID)), job.getLog().toString());
		}
		finally {
			dropConstraint(AAP_TABLE);
		}
	}

	@Test
	void testPerformDocumentActionForTagActsOnTheRestAfterAFailure() throws Exception {
		List<Bean> beans = saveAaps();
		addConstraint(AAP_TABLE, "text <> '" + FAILING_TEXT + "'");
		try {
			PerformDocumentActionForTagJob job = tagActionJob(TagDefaultAction.tagUpsert, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME, beans);

			job.execute();
			commit();

			assertEquals(List.of("1 before|1|null", FAILING_TEXT + "|0|null", "3 after|1|null", "4 last|1|null"), aapRows());
			assertEquals(1, unsuccessful(job.getLog()), job.getLog().toString());
		}
		finally {
			dropConstraint(AAP_TABLE);
		}
	}

	/**
	 * A user proxy's bizKey goes through its contact, a lazy association, so after the rollback detaches the proxies
	 * still to come, they can only be logged and upserted once they are retrieved again.
	 */
	@Test
	void testPerformDocumentActionForTagRetrievesTheItemsAfterAFailure() throws Exception {
		Persistence p = CORE.getPersistence();
		for (int i = 1; i <= 3; i++) {
			UserExtension user = new DataBuilder().fixture(FixtureType.crud).build(AdminDocument.USER);
			user.setBizId("rbo-" + i);
			user.setUserName(i + " user");
			user.setPassword(EXT.hashPassword("TestPassword0!"));
			user.setPasswordHistory(null);
			int groupNumber = 0;
			for (GroupExtension group : user.getGroups()) {
				group.setName("rbo group " + i + '.' + (++groupNumber));
			}
			p.save(user);
		}
		commit();
		// The upserts will add 1 to the versions of the first and last
		List<String> expected = new ArrayList<>(userRows(1));
		expected.set(1, userRows(0).get(1));
		addConstraint(USER_TABLE, "userName <> '2 user'");
		try {
			List<Bean> proxies = new ArrayList<>();
			for (int i = 1; i <= 3; i++) {
				proxies.add(p.retrieve(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME, "rbo-" + i));
			}
			PerformDocumentActionForTagJob job = tagActionJob(TagDefaultAction.tagUpsert, UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME, proxies);

			job.execute();
			commit();

			assertEquals(expected, userRows(0));
			assertEquals(1, unsuccessful(job.getLog()), job.getLog().toString());
		}
		finally {
			dropConstraint(USER_TABLE);
		}
	}

	@Test
	void testPerformDocumentActionForTagLogsAnItemDeletedBeforeItsTurn() throws Exception {
		List<Bean> beans = saveAaps();
		// Deleted after the job was given its items
		Persistence p = CORE.getPersistence();
		p.newSQL("delete from " + AAP_TABLE + " where bizId = 'rbo-3'").execute();
		p.commit(false);
		p.begin();
		addConstraint(AAP_TABLE, "text <> '" + FAILING_TEXT + "'");
		try {
			PerformDocumentActionForTagJob job = tagActionJob(TagDefaultAction.tagUpsert, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME, beans);

			job.execute();
			commit();

			assertEquals(List.of("1 before|1|null", FAILING_TEXT + "|0|null", "4 last|1|null"), aapRows());
			assertEquals(2, unsuccessful(job.getLog()), job.getLog().toString());
		}
		finally {
			dropConstraint(AAP_TABLE);
		}
	}

	/**
	 * A failure that is not in the database leaves the transaction healthy, so its work was committed with the next
	 * item: the item was untagged although it was not deleted.
	 */
	@Test
	void testPerformDocumentActionForTagKeepsTheTagOfAnItemThatCannotBeDeleted() throws Exception {
		saveAaps();
		saveReferrerOfTheFailingItem();
		TagExtension tag = saveTag(retrieveAaps());
		List<String> before = aapRows();
		// The job removes an item from the tag and then deletes it
		PerformDocumentActionForTagJob job = new PerformDocumentActionForTagJob();
		job.setBean(tag);
		tag.setDocumentAction(TagDefaultAction.tagDelete.toCode());
		tag.setActionModuleName(AllAttributesPersistent.MODULE_NAME);
		tag.setActionDocumentName(AllAttributesPersistent.DOCUMENT_NAME);
		tag.setEvictOption(EvictOption.none);
		injectTagService(job, PerformDocumentActionForTagJob.class, tag, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME, retrieveAaps());

		job.execute();
		commit();

		assertEquals(before.stream().filter(r -> r.startsWith(FAILING_TEXT) || r.startsWith("referrer")).toList(), aapRows());
		assertEquals(List.of(FAILING_ID), taggedIds(tag));
	}

	@Test
	void testDeleteAllTaggedDataDeletesTheRestAfterADatabaseFailure() throws Exception {
		TagExtension tag = saveTag(saveAaps());
		try {
			block(AAP_TABLE, FAILING_ID);
			DeleteAllTaggedDataForTagJob job = new DeleteAllTaggedDataForTagJob();
			job.setBean(tag);
			injectTagService(job, DeleteAllTaggedDataForTagJob.class, tag, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME, retrieveAaps());

			job.execute();
			commit();

			assertEquals(List.of(FAILING_TEXT + "|0|null"), aapRows());
			// Its untag was rolled back with its delete
			assertEquals(List.of(FAILING_ID), taggedIds(tag));
		}
		finally {
			discard();
			ddl("drop table if exists " + BLOCKER_TABLE);
		}
	}

	/**
	 * A failure that is not in the database leaves the transaction healthy, so its work was committed with the next
	 * item: the item was untagged although it was not deleted.
	 */
	@Test
	void testDeleteAllTaggedDataKeepsTheTagOfAnItemThatCannotBeDeleted() throws Exception {
		saveAaps();
		saveReferrerOfTheFailingItem();
		TagExtension tag = saveTag(retrieveAaps());
		List<String> before = aapRows();
		DeleteAllTaggedDataForTagJob job = new DeleteAllTaggedDataForTagJob();
		job.setBean(tag);
		injectTagService(job, DeleteAllTaggedDataForTagJob.class, tag, AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME, retrieveAaps());

		job.execute();
		commit();

		assertEquals(before.stream().filter(r -> r.startsWith(FAILING_TEXT) || r.startsWith("referrer")).toList(), aapRows());
		assertEquals(List.of(FAILING_ID), taggedIds(tag));
	}

	@Test
	void testGenerateTestDataKeepsTheTagAndTheItemsAfterFailures() throws Exception {
		// Every test document of this kind fails in the database.
		// Neither document has associations, so the job's data builder only makes the documents themselves.
		addConstraint(UNIQUE_TABLE, "1 = 0");
		try {
			ControlPanelExtension bean = ControlPanel.newInstance();
			bean.getTestDocumentNames().add(moduleDocument(UniqueConstraintNonNullable.MODULE_NAME, UniqueConstraintNonNullable.DOCUMENT_NAME));
			bean.getTestDocumentNames().add(moduleDocument(Hierarchical.MODULE_NAME, Hierarchical.DOCUMENT_NAME));
			bean.setTestNumberToGenerate(Integer.valueOf(2));
			bean.setTestTagGeneratedData(Boolean.TRUE);
			bean.setTestTagName("rbo generated");
			GenerateTestDataJob job = new GenerateTestDataJob();
			job.setBean(bean);

			job.execute();
			commit();

			assertTrue(job.getLog().contains("2 Documents successfully created, 2 failed."), job.getLog().toString());
			Persistence p = CORE.getPersistence();
			Tag tag = p.newDocumentQuery(AdminDocument.TAG).beanResult();
			assertEquals("rbo generated", (tag == null) ? null : tag.getName());
			List<String> tagged = taggedIds(tag);
			assertEquals(2, tagged.size());
			for (String bizId : tagged) {
				assertNotNull(p.retrieve(Hierarchical.MODULE_NAME, Hierarchical.DOCUMENT_NAME, bizId), bizId);
			}
		}
		finally {
			dropConstraint(UNIQUE_TABLE);
		}
	}

	private static RefreshDocumentTuplesJob refreshJob(Boolean notification) {
		DataMaintenanceExtension dm = DataMaintenance.newInstance();
		dm.setNotification(notification);
		dm.setRefreshOption(RefreshOption.upsert);
		dm.setEvictOption(EvictOption.none);
		dm.setFlagFailed(Boolean.TRUE);
		dm.getRefreshDocuments().add(moduleDocument(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME));
		RefreshDocumentTuplesJob result = new RefreshDocumentTuplesJob();
		result.setBean(dm);
		return result;
	}

	private static PerformDocumentActionForTagJob tagActionJob(TagDefaultAction action, String moduleName, String documentName, List<Bean> beans)
	throws Exception {
		TagExtension tag = Tag.newInstance();
		tag.setDocumentAction(action.toCode());
		tag.setActionModuleName(moduleName);
		tag.setActionDocumentName(documentName);
		tag.setEvictOption(EvictOption.none);
		tag.setNotification(Boolean.FALSE);
		PerformDocumentActionForTagJob result = new PerformDocumentActionForTagJob();
		result.setBean(tag);
		injectTagService(result, PerformDocumentActionForTagJob.class, tag, moduleName, documentName, beans);
		return result;
	}

	/** Save the items, committed, with bizIds in the same order as their texts. */
	private static List<Bean> saveAaps() {
		Persistence p = CORE.getPersistence();
		for (int i = 0, l = TEXTS.size(); i < l; i++) {
			AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
			bean.setBizId("rbo-" + (i + 1));
			bean.setText(TEXTS.get(i));
			p.save(bean);
		}
		commit();
		return retrieveAaps();
	}

	/** The items, in order, attached to the current transaction, as a tag service gives them to a job. */
	private static List<Bean> retrieveAaps() {
		Persistence p = CORE.getPersistence();
		List<Bean> result = new ArrayList<>();
		for (int i = 1, l = TEXTS.size(); i <= l; i++) {
			Bean bean = p.retrieve(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME, "rbo-" + i);
			if (bean != null) {
				result.add(bean);
			}
		}
		return result;
	}

	/** Make Skyve refuse to delete the failing item, outside the database, because another bean refers to it. */
	private static void saveReferrerOfTheFailingItem() {
		Persistence p = CORE.getPersistence();
		AllAttributesPersistent referrer = AllAttributesPersistent.newInstance();
		referrer.setText("referrer");
		referrer.setAggregatedAssociation(p.retrieve(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME, FAILING_ID));
		p.save(referrer);
		commit();
	}

	private static TagExtension saveTag(List<Bean> beans) throws Exception {
		Persistence p = CORE.getPersistence();
		TagExtension tag = Tag.newInstance();
		tag.setName("rbo tag");
		tag.setVisible(Boolean.TRUE);
		tag.setNotification(Boolean.FALSE);
		tag = p.save(tag);
		for (Bean bean : beans) {
			EXT.getTagManager().tag(tag.getBizId(), bean);
		}
		commit();
		return tag;
	}

	private static ModuleDocument moduleDocument(String moduleName, String documentName) {
		ModuleDocument result = ModuleDocument.newInstance();
		result.setInclude(Boolean.TRUE);
		result.setModuleName(moduleName);
		result.setDocumentName(documentName);
		return result;
	}

	/** Give the job a tag service that returns the beans, in order, for the document. */
	private static void injectTagService(Object job, Class<?> jobClass, TagExtension tag, String moduleName, String documentName, List<Bean> beans)
	throws Exception {
		TagService tagService = mock(TagService.class);
		when(tagService.getTaggedItemsForDocument(any(), any(), any())).thenReturn(List.of());
		when(tagService.getTaggedItemsForDocument(tag, moduleName, documentName)).thenReturn(beans);
		Field field = jobClass.getDeclaredField("tagService");
		field.setAccessible(true);
		field.set(job, tagService);
	}

	/** Make writing a row fail in the database, unless the condition holds; existing rows are not checked. */
	private static void addConstraint(String table, String condition) {
		ddl("alter table " + table + " add constraint " + CONSTRAINT + " check (" + condition + ") nocheck");
	}

	private static void dropConstraint(String table) {
		discard();
		ddl("alter table " + table + " drop constraint if exists " + CONSTRAINT);
	}

	/** Discard what a failed test left, since H2 commits it with the next DDL statement. */
	private static void discard() {
		Persistence p = CORE.getPersistence();
		p.rollback();
		p.begin();
	}

	/** Make deleting the row fail in the database, with a foreign key that Skyve does not know about. */
	private static void block(String table, String bizId) {
		ddl("create table " + BLOCKER_TABLE + " (id varchar(36) primary key, ref varchar(36) references " + table + "(bizId))");
		Persistence p = CORE.getPersistence();
		p.newSQL("insert into " + BLOCKER_TABLE + " (id, ref) values ('blocker', :ref)").putParameter("ref", bizId, false).execute();
		commit();
	}

	private static void ddl(String statement) {
		CORE.getPersistence().newSQL(statement).execute();
		commit();
	}

	private static void commit() {
		Persistence p = CORE.getPersistence();
		p.commit(false);
		p.evictAllCached();
		p.begin();
	}

	private static long unsuccessful(List<String> log) {
		return log.stream().filter(l -> l.contains(UNSUCCESSFUL)).count();
	}

	/** userName|bizVersion (plus the increment) of the test's users, in userName order. */
	private static List<String> userRows(int increment) {
		List<String> result = new ArrayList<>();
		for (Object[] row : CORE.getPersistence().newSQL("select userName, bizVersion from " + USER_TABLE + " where bizId like 'rbo-%' order by userName").tupleResults()) {
			result.add(row[0] + "|" + (((Number) row[1]).intValue() + increment));
		}
		return result;
	}

	/** text|bizVersion|bizFlagComment of the committed rows, in text order. */
	private static List<String> aapRows() {
		List<String> result = new ArrayList<>();
		for (Object[] row : CORE.getPersistence().newSQL("select text, bizVersion, bizFlagComment from " + AAP_TABLE + " order by text").tupleResults()) {
			result.add(row[0] + "|" + row[1] + "|" + row[2]);
		}
		return result;
	}

	private static List<String> taggedIds(Tag tag) throws Exception {
		List<String> result = new ArrayList<>();
		if (tag != null) {
			try (AutoClosingIterable<Bean> tagged = EXT.getTagManager().iterate(tag.getBizId())) {
				for (Bean bean : tagged) {
					result.add(bean.getBizId());
				}
			}
		}
		result.sort(null);
		return result;
	}
}
