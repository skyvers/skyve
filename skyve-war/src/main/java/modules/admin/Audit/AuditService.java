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
import org.skyve.util.logging.SkyveLoggerFactory;

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
	@SuppressWarnings("java:S6813") // allow member injection
	private transient Persistence persistence;

	private ArchiveRetriever retriever = ArchiveRetriever.getInstance();

	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(AuditService.class);

	/**
	 * Retrieves an audit record from the relational database by business identifier.
	 *
	 * @param bizId the business identifier of the audit record to retrieve; never {@code null}
	 * @return the database audit record, or {@code null} if no live row exists
	 */
	public Audit retrieveFromRdbms(String bizId) {
		return persistence.retrieve(Audit.MODULE_NAME, Audit.DOCUMENT_NAME, bizId);
	}

	/**
	 * Retrieves an audit record from the archive store when archiving is configured.
	 *
	 * <p>If archiving is not enabled for the audit document, the method returns
	 * {@code null} without attempting a lookup.
	 *
	 * @param bizId the business identifier of the audit record to retrieve; never {@code null}
	 * @return the archived audit record, or {@code null} if archiving is disabled or the record is absent
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
	 * Retrieves the available versions for an audit record from both the database and the archive.
	 *
	 * <p>The combined result is deduplicated, sorted newest-first, and converted into domain
	 * values suitable for selection controls.
	 *
	 * @param bean the audit record whose versions are being enumerated; never {@code null}
	 * @param forComparison {@code true} to return only versions older than the selected source audit
	 * @return the available version choices, never {@code null}
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
	 * Retrieves audit versions from the relational database for the supplied audit record.
	 *
	 * <p>When {@code forComparison} is {@code true}, the method returns only records older than
	 * the selected audit and suppresses comparison lookup for non-update operations.
	 *
	 * @param audit the audit record to find versions for; never {@code null}
	 * @param forComparison {@code true} to limit the result to earlier versions
	 * @return the database versions, limited to 100 rows; never {@code null}
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
	 * Retrieves audit versions from the archive index for the supplied audit record.
	 *
	 * <p>When archiving is disabled, the method returns an empty list. When
	 * {@code forComparison} is {@code true}, only older audit versions are retained.
	 *
	 * @param audit the audit record to find versions for; never {@code null}
	 * @param forComparison {@code true} to limit the result to earlier versions
	 * @return the archived versions, limited to 500 rows; never {@code null}
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
	 * Merges archive and database audit lists while preferring archived rows on duplicate identifiers.
	 *
	 * @param luceneVersions the archived audit rows; never {@code null}
	 * @param rdbmsVersions the relational audit rows; never {@code null}
	 * @return the merged list of unique audit rows, never {@code null}
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
	 * Sorts the supplied audit beans newest-first and removes duplicate insert rows.
	 *
	 * <p>The oldest record is preserved even when it is an insert so the comparison
	 * UI always has a terminal baseline row.
	 *
	 * @param audits the audit rows to normalise; must be mutable and never {@code null}
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
	 * Recomputes the comparison-version selection after the source-version choice changes.
	 *
	 * <p>The first eligible comparison version is selected automatically when a matching
	 * source update exists; otherwise the comparison selection is cleared.
	 *
	 * @param bean the audit bean whose source-version selection changed; never {@code null}
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
	 * Converts audit beans into selection values that expose the business identifier and business key.
	 *
	 * @param versions the audit versions to convert; never {@code null}
	 * @return the selection values for the supplied rows, never {@code null}
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
	 * Resolves the archive configuration for the audit document.
	 *
	 * @return the audit archive configuration when present, or an empty optional when archiving is disabled
	 */
	@SuppressWarnings("static-method")
	public Optional<ArchiveDocConfig> auditDocConfig() {

		return Util.getArchiveConfig()
				.findArchiveDocConfig(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
	}
}
