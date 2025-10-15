package modules.admin.Audit;

import static java.util.Collections.emptyList;
import static java.util.Comparator.comparing;
import static java.util.stream.Collectors.toSet;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.archive.list.LuceneFilter;
import org.skyve.impl.archive.support.ArchiveRetriever;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveDocConfig;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.inject.Inject;
import modules.admin.domain.Audit;
import modules.admin.domain.Audit.Operation;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient AuditService auditService;
 */
public class AuditService {
	@Inject
	private Persistence persistence;
	private ArchiveRetriever retriever = ArchiveRetriever.getInstance();

	private static final Logger LOGGER = LoggerFactory.getLogger(AuditService.class);

	/**
	 * Retrieves an Audit record from the relational database by its BizID.
	 * 
	 * @param bizId The BizID of the Audit record to retrieve
	 * @return The Audit record, or null if not found
	 */
	public Audit retrieveFromRdbms(String bizId) {
		return persistence.retrieve(Audit.MODULE_NAME, Audit.DOCUMENT_NAME, bizId);
	}

	/**
	 * Retrieve the request Audit from the archives (if Audit archiving is
	 * enabled).
	 * 
	 * @return The requested bean, or null if it isn't found or if Audit
	 *         archiving isn't configured.
	 */
	public Audit retrieveFromArchives(String bizId) {

		Optional<ArchiveDocConfig> config = auditDocConfig();
		if (config.isEmpty()) {
			return null;
		}

		return retriever.<Audit> retrieveByBizId(config.get(), bizId)
				.orElse(null);
	}

	/**
	 * Retrieves all available versions of an audit record from both the database and archives.
	 * The method combines results from both sources, removes duplicate insert operations,
	 * and returns them as domain values suitable for UI selection components.
	 * 
	 * @param bean The audit record to find versions for
	 * @param forComparison If true, only returns versions that occurred before the given audit record (for comparison purposes)
	 * @return A list of domain values representing available audit versions
	 */
	public List<DomainValue> getVersions(Audit bean, boolean forComparison) {

		LOGGER.trace("Getting versions for {} [forComparison:{}]", bean, Boolean.valueOf(forComparison));

		List<Audit> luceneVersions = getLuceneVersions(bean, forComparison);
		List<Bean> rdbmsVersions = getRdbmsVersions(bean, forComparison);
		List<Bean> merged = merge(luceneVersions, rdbmsVersions);

		// Sort & delete spurious 'insert' records
		cleanSortAuditList(merged);

		return convertToDomainValues(merged);
	}

	/**
	 * Retrieves audit record versions from the relational database for a given audit record.
	 * When used for comparison, only returns records that occurred before the specified audit.
	 * 
	 * @param audit The audit record to find versions for
	 * @param forComparison If true, only returns versions that occurred before the given audit record
	 * @return A list of audit record versions from the database, limited to 100 results
	 */
	@SuppressWarnings("static-method")
	public List<Bean> getRdbmsVersions(Audit audit, boolean forComparison) {

		if (forComparison && (!Operation.update.equals(audit.getOperation()))) {
			return new ArrayList<>();
		}

		Persistence p = CORE.getPersistence();

		DocumentQuery q = p.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
		q.addBoundProjection(Bean.DOCUMENT_ID);
		q.addBoundProjection(Bean.BIZ_KEY);
		q.addBoundProjection(Audit.millisPropertyName);
		q.addBoundProjection(Audit.operationPropertyName);

		DocumentFilter f = q.getFilter();
		f.addEquals(Audit.auditBizIdPropertyName, audit.getAuditBizId());
		if (forComparison) {
			f.addLessThan(Audit.millisPropertyName, audit.getMillis());
		}
		q.addBoundOrdering(Audit.millisPropertyName, SortDirection.descending);

		q.setFirstResult(0)
				.setMaxResults(100);
		List<Bean> versions = q.projectedResults();

		return versions;
	}

	/**
	 * Retrieves audit record versions from the Lucene for a given audit record.
	 * When used for comparison, only returns records that occurred before the specified audit.
	 * Returns an empty list if audit archiving is not configured.
	 * 
	 * @param audit The audit record to find versions for
	 * @param forComparison If true, only returns versions that occurred before the given audit record
	 * @return A list of audit record versions from the archives, limited to 500 results
	 */
	@SuppressWarnings("boxing")
	public List<Audit> getLuceneVersions(Audit audit, boolean forComparison) {
		Optional<ArchiveDocConfig> config = auditDocConfig();
		if (config.isEmpty()) {
			LOGGER.debug("Audit archiving not configured");
			return emptyList();
		}

		if (forComparison && (!Operation.update.equals(audit.getOperation()))) {
			return new ArrayList<>();
		}

		LuceneFilter filter = new LuceneFilter();
		filter.addEquals(Audit.auditBizIdPropertyName, audit.getAuditBizId());

		List<Audit> audits = new ArrayList<>(retriever.retrieveAll(config.get(), filter, 500));

		if (forComparison) {
			audits.removeIf(a -> a.getMillis() >= audit.getMillis());
		}

		return audits;
	}

	/**
	 * Merges two lists of audit records - one from the Lucene archive system and one from the RDBMS.
	 * Eliminates duplicates by preferring Lucene versions over RDBMS versions when the same bizID exists in both lists.
	 * 
	 * @param luceneVersions List of audit records from the Lucene archive system
	 * @param rdbmsVersions List of audit records from the relational database
	 * @return A merged list containing all unique audit records, with Lucene versions taking precedence over RDBMS duplicates
	 */
	private static List<Bean> merge(List<Audit> luceneVersions, List<Bean> rdbmsVersions) {

		List<Bean> mergedResult = new ArrayList<>(luceneVersions);
		Set<String> luceneAuditIds = luceneVersions.stream()
				.map(Audit::getBizId)
				.collect(toSet());

		for (Bean dbAudit : rdbmsVersions) {

			String bizId = (String) Binder.get(dbAudit, Bean.DOCUMENT_ID);
			if (!luceneAuditIds.contains(bizId)) {
				mergedResult.add(dbAudit);
			}
		}

		return mergedResult;
	}

	/**
	 * Remove all but the oldest Insert operations and sort the
	 * provided list of audits newest to oldest.
	 * 
	 * @param audits The list of audits, must be sorted so the older audit
	 *        is at the end of the list
	 */
	@SuppressWarnings("static-method")
	private void cleanSortAuditList(List<Bean> audits) {

		if (audits.size() <= 1) {
			return;
		}

		// Sort the supplied list, with oldest items (ie smallest millis value) at the end
		Function<Bean, Long> millisFn = bean -> {
			if (bean instanceof Audit a) {
				return a.getMillis();
			}

			return (Long) Binder.get(bean, Audit.millisPropertyName);
		};

		Collections.sort(audits, comparing(millisFn).reversed());

		// This will be the oldest audit record
		Bean lastElement = audits.get(audits.size() - 1);

		// Remove all 'insert' Audits, except the first
		for (Iterator<Bean> it = audits.iterator(); it.hasNext();) {
			Bean bean = it.next();

			// Always leave the last element alone
			// This should be true insert record
			if (bean == lastElement) {
				continue;
			}

			Operation op = (Operation) Binder.get(bean, Audit.operationPropertyName);
			if (op == Operation.insert) {
				it.remove();
			}
		}
	}

	/**
	 * The "Source Version To Compare" selection has changed, update the available
	 * comparison versions options ("Other Version To Compare"), picking the first option
	 * as a default.
	 * 
	 * @param bean The audit record containing the source version selection and where the comparison version will be set
	 */
	public void sourceVersionChanged(Audit bean) {
		Audit source = bean.getSourceVersion();
		bean.setMe(bean);

		if (Operation.update.equals(source.getOperation())) {
			List<DomainValue> lesserVersions = getVersions(source, true);
			if (lesserVersions.isEmpty()) {
				bean.setComparisonVersion(null);
			} else {
				// Set the next Audit in the versions list to compare to
				String bizId = lesserVersions.get(0)
						.getCode();

				// Try searching the RDBMS first
				Audit found = retrieveFromRdbms(bizId);

				// Then try the archives
				if (found == null) {
					found = retrieveFromArchives(bizId);
				}

				bean.setComparisonVersion(found);
			}
		} else {
			bean.setComparisonVersion(null);
		}
	}

	/**
	 * Converts a list of audit record beans into DomainValue objects suitable for use in UI selection components.
	 * Each DomainValue contains the bizID as the code and the bizKey as the display value.
	 * 
	 * @param versions The list of audit record beans to convert
	 * @return A list of DomainValue objects containing the bizID and bizKey from each audit record
	 */
	private static List<DomainValue> convertToDomainValues(List<Bean> versions) {

		List<DomainValue> result = new ArrayList<>(versions.size());

		for (Bean version : versions) {
			String bizId = (String) Binder.get(version, Bean.DOCUMENT_ID);
			String bizKey = (String) Binder.get(version, Bean.BIZ_KEY);

			result.add(new DomainValue(bizId, bizKey));
		}

		return result;
	}

	/**
	 * Find the Audit document archiving configuration (if it exists).
	 * 
	 * @return An Optional containing the ArchiveDocConfig for Audit documents, or empty if archiving is not configured
	 */
	@SuppressWarnings("static-method")
	public Optional<ArchiveDocConfig> auditDocConfig() {

		return Util.getArchiveConfig()
				.findArchiveDocConfig(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
	}
}
