package modules.admin.Audit;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

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

class AuditServiceH2Test extends AbstractH2Test {
	@Test
	@SuppressWarnings("static-method")
	void mergePrefersArchiveVersionsWhenDatabaseContainsSameBizId() throws Exception {
		Audit archivedDuplicate = audit("same", "Archived duplicate", 30L, Operation.update);
		Audit archivedOnly = audit("archived", "Archived only", 20L, Operation.delete);
		Bean databaseDuplicate = projectedAudit("same", "Database duplicate", 25L, Operation.update);
		Bean databaseOnly = projectedAudit("database", "Database only", 10L, Operation.insert);

		List<Bean> merged = invokeMerge(
				List.of(archivedDuplicate, archivedOnly),
				List.of(databaseDuplicate, databaseOnly));

		assertThat(merged.size(), is(3));
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

		assertThat(audits.size(), is(3));
		assertThat(audits.get(0), is(newestUpdate));
		assertThat(audits.get(1), is(olderUpdate));
		assertThat(audits.get(2), is(oldestInsert));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertToDomainValuesUsesDocumentIdAndBizKey() throws Exception {
		List<DomainValue> values = invokeConvertToDomainValues(List.of(
				projectedAudit("audit-1", "First audit", 20L, Operation.update),
				projectedAudit("audit-2", "Second audit", 10L, Operation.delete)));

		assertThat(values.size(), is(2));
		assertThat(values.get(0).getCode(), is("audit-1"));
		assertThat(values.get(0).getLocalisedDescription(), is("First audit"));
		assertThat(values.get(1).getCode(), is("audit-2"));
		assertThat(values.get(1).getLocalisedDescription(), is("Second audit"));
	}

	private static Audit audit(String bizId, String bizKey, long millis, Operation operation) throws Exception {
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
}
