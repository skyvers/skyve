package modules.admin.Audit;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

import modules.admin.domain.Audit;
import modules.admin.domain.Audit.Operation;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class AuditServiceH2Test extends AbstractH2Test {
	@Test
	void mergePrefersArchiveVersionsWhenDatabaseContainsSameBizId() throws Exception {
		Audit archivedDuplicate = audit("same", "Archived duplicate", 30L, Operation.update);
		Audit archivedOnly = audit("archived", "Archived only", 20L, Operation.delete);
		Bean databaseDuplicate = projectedAudit("same", "Database duplicate", 25L, Operation.update);
		Bean databaseOnly = projectedAudit("database", "Database only", 10L, Operation.insert);

		List<Bean> merged = invokeMerge(
				List.of(archivedDuplicate, archivedOnly),
				List.of(databaseDuplicate, databaseOnly));

		assertEquals(3, merged.size());
		assertThat(merged.get(0), is(archivedDuplicate));
		assertThat(merged.get(1), is(archivedOnly));
		assertThat(merged.get(2), is(databaseOnly));
	}

	@Test
	void cleanSortAuditListSortsNewestFirstAndRemovesNonTerminalInserts() throws Exception {
		Audit newestUpdate = audit("newest-update", "Newest update", 40L, Operation.update);
		Audit middleInsert = audit("middle-insert", "Middle insert", 30L, Operation.insert);
		Bean olderUpdate = projectedAudit("older-update", "Older update", 20L, Operation.update);
		Bean oldestInsert = projectedAudit("oldest-insert", "Oldest insert", 10L, Operation.insert);
		List<Bean> audits = new ArrayList<>(List.of(olderUpdate, middleInsert, oldestInsert, newestUpdate));

		invokeCleanSortAuditList(new AuditService(), audits);

		assertEquals(3, audits.size());
		assertThat(audits.get(0), is(newestUpdate));
		assertThat(audits.get(1), is(olderUpdate));
		assertThat(audits.get(2), is(oldestInsert));
	}

	@Test
	void convertToDomainValuesUsesDocumentIdAndBizKey() throws Exception {
		List<DomainValue> values = invokeConvertToDomainValues(List.of(
				projectedAudit("audit-1", "First audit", 20L, Operation.update),
				projectedAudit("audit-2", "Second audit", 10L, Operation.delete)));

		assertEquals(2, values.size());
		assertThat(values.get(0).getCode(), is("audit-1"));
		assertThat(values.get(0).getLocalisedDescription(), is("First audit"));
		assertThat(values.get(1).getCode(), is("audit-2"));
		assertThat(values.get(1).getLocalisedDescription(), is("Second audit"));
	}

	@Test
	void retrieveFromArchivesReturnsNullWhenArchiveIsNotConfigured() {
		assertNull(new AuditService().retrieveFromArchives("missing-audit"));
	}

	@Test
	void sourceVersionChangedClearsComparisonForNonUpdateSource() {
		Audit bean = audit("bean", "Bean", 30L, Operation.update);
		Audit source = audit("source", "Source", 20L, Operation.delete);
		Audit comparison = audit("comparison", "Comparison", 10L, Operation.insert);
		bean.setSourceVersion(source);
		bean.setComparisonVersion(comparison);

		new AuditService().sourceVersionChanged(bean);

		assertThat(bean.getMe(), is(bean));
		assertNull(bean.getComparisonVersion());
	}

	@Test
	void sourceVersionChangedUsesFirstLesserVersionFromRdbms() {
		Audit bean = audit("bean", "Bean", 40L, Operation.update);
		Audit source = audit("source", "Source", 30L, Operation.update);
		Audit comparison = audit("comparison", "Comparison", 20L, Operation.update);
		bean.setSourceVersion(source);
		TestAuditService service = new TestAuditService(List.of(new DomainValue("comparison", "Comparison")), comparison);

		service.sourceVersionChanged(bean);

		assertThat(bean.getComparisonVersion(), is(comparison));
	}

	@Test
	void sourceVersionChangedClearsComparisonWhenNoLesserVersionsExist() {
		Audit bean = audit("bean", "Bean", 40L, Operation.update);
		Audit source = audit("source", "Source", 30L, Operation.update);
		Audit comparison = audit("comparison", "Comparison", 20L, Operation.update);
		bean.setSourceVersion(source);
		bean.setComparisonVersion(comparison);

		new TestAuditService(List.of(), comparison).sourceVersionChanged(bean);

		assertNull(bean.getComparisonVersion());
	}

	private static Audit audit(String bizId, String bizKey, long millis, Operation operation) {
		Audit audit = Audit.newInstance();
		audit.setBizId(bizId);
		audit.setBizKey(bizKey);
		audit.setMillis(Long.valueOf(millis));
		audit.setOperation(operation);
		return audit;
	}

	private static Bean projectedAudit(String bizId, String bizKey, long millis, Operation operation) {
		Map<String, Object> properties = new TreeMap<>();
		properties.put(Bean.DOCUMENT_ID, bizId);
		properties.put(Bean.BIZ_KEY, bizKey);
		properties.put(Audit.millisPropertyName, Long.valueOf(millis));
		properties.put(Audit.operationPropertyName, operation);
		return new DynamicBean(Audit.MODULE_NAME, Audit.DOCUMENT_NAME, properties);
	}

	@SuppressWarnings("unchecked")
	private static List<Bean> invokeMerge(List<Audit> luceneVersions, List<Bean> rdbmsVersions) throws Exception {
		Method method = AuditService.class.getDeclaredMethod("merge", List.class, List.class);
		method.setAccessible(true);
		return (List<Bean>) method.invoke(null, luceneVersions, rdbmsVersions);
	}

	private static void invokeCleanSortAuditList(AuditService service, List<Bean> audits) throws Exception {
		Method method = AuditService.class.getDeclaredMethod("cleanSortAuditList", List.class);
		method.setAccessible(true);
		method.invoke(service, audits);
	}

	@SuppressWarnings("unchecked")
	private static List<DomainValue> invokeConvertToDomainValues(List<Bean> versions) throws Exception {
		Method method = AuditService.class.getDeclaredMethod("convertToDomainValues", List.class);
		method.setAccessible(true);
		return (List<DomainValue>) method.invoke(null, versions);
	}

	private static final class TestAuditService extends AuditService {
		private final List<DomainValue> versions;
		private final Audit rdbmsAudit;

		private TestAuditService(List<DomainValue> versions, Audit rdbmsAudit) {
			this.versions = versions;
			this.rdbmsAudit = rdbmsAudit;
		}

		@Override
		public List<DomainValue> getVersions(Audit bean, boolean forComparison) {
			return versions;
		}

		@Override
		public Audit retrieveFromRdbms(String bizId) {
			return rdbmsAudit;
		}
	}
}
