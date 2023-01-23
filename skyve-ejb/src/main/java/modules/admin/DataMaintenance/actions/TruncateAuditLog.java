package modules.admin.DataMaintenance.actions;

import java.util.ArrayList;
import java.util.List;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;
import org.skyve.util.Binder;
import org.skyve.web.WebContext;

import modules.admin.domain.Audit;
import modules.admin.domain.Audit.Operation;
import modules.admin.domain.DataMaintenance;

public class TruncateAuditLog implements ServerSideAction<DataMaintenance> {
	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
			throws Exception {

		Persistence pers = CORE.getPersistence();
		User user = pers.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(DataMaintenance.MODULE_NAME);
		JobMetaData job = module.getJob("jTruncateAuditLog");

		EXT.getJobScheduler().runOneShotJob(job, bean, user);

		bean.setAuditResponse("Truncate Audit Log Job commenced.");

		return new ServerSideActionResult<>(bean);
	}

	/**
	 * Helper method for the TruncateAuditLogJob
	 * 
	 * @param pers
	 * @param millisStart
	 * @param millisEnd
	 * @return DocumentQuery with a days worth of Audits to check
	 */
	public static DocumentQuery retrieveDaysAudits(Persistence pers, long millisStart, long millisEnd) {
		DocumentQuery q = pers.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);
		q.getFilter().addBetween(Audit.millisPropertyName, Long.valueOf(millisEnd), Long.valueOf(millisStart));
		q.addBoundProjection(Bean.DOCUMENT_ID);
		q.addBoundProjection(Audit.auditBizIdPropertyName);
		q.addBoundProjection(Audit.millisPropertyName);
		q.addBoundProjection(Audit.operationPropertyName);

		return q;
	}

	/**
	 * Helper method for the TruncateAuditLogJob
	 * 
	 * Uses DocumentQuery to retrieve the audits to potentially be truncated
	 * These Audits are then processed
	 * 
	 * @param pers
	 * @param audit
	 * @return List of Audit BizIds related to the Audit to be truncated
	 * @throws Exception
	 */
	public static List<String> retrieveAuditsToTruncate(Persistence pers, Bean audit) throws Exception {
		List<String> auditBizIdsToTruncate = new ArrayList<>();

		// Checking all Audits with same auditBizId of the Audit passed in
		DocumentQuery q = pers.newDocumentQuery(Audit.MODULE_NAME, Audit.DOCUMENT_NAME);

		// If Audit is a Delete record, all instances with the same auditBizId can be truncated
		Operation operation = (Operation) Binder.get(audit, Audit.operationPropertyName);
		if (Operation.delete.equals(operation)) {
			q.getFilter().addEquals(Audit.auditBizIdPropertyName, Binder.get(audit, Audit.auditBizIdPropertyName));
			q.addBoundProjection(Bean.DOCUMENT_ID);
			q.addBoundProjection(Audit.auditBizIdPropertyName);
			q.addBoundProjection(Audit.millisPropertyName);
			q.addBoundProjection(Audit.operationPropertyName);

			try (AutoClosingIterable<Audit> audits = q.beanIterable()) {
				for (Bean processedAudit : audits) {
					auditBizIdsToTruncate.add(processedAudit.getBizId());
				}
			}
			return auditBizIdsToTruncate;
		}

		// Reconstruction records should all be deleted, regardless of whether they are the penultimate record
		if (Operation.reconstruction.equals(operation)) {
			auditBizIdsToTruncate.add(audit.getBizId());
			return auditBizIdsToTruncate;
		}

		// Insert and Update Audit records need to be checked that they aren't the penultimate record
		// and that they are the Audit passed in or were earlier
		// Reconstruction records should all be deleted
		q.getFilter().addEquals(Audit.auditBizIdPropertyName, Binder.get(audit, Audit.auditBizIdPropertyName));
		q.addBoundProjection(Bean.DOCUMENT_ID);
		q.addBoundProjection(Audit.auditBizIdPropertyName);
		q.addBoundProjection(Audit.millisPropertyName);
		q.addBoundProjection(Audit.operationPropertyName);
		q.addBoundOrdering(Audit.millisPropertyName, SortDirection.descending);
		
		try (AutoClosingIterable<Bean> audits = q.beanIterable()) {
			// The List is used to check the number of Audits retrieved
			List<Audit> indexedAudits = q.beanResults();
			Audit penultimateAudit = indexedAudits.get(1);

			// Insert and Update Audit records will not be truncated when they are the penultimate or ultimate record
			// therefore at least 3 Audits for the same auditBizid are required to truncate 1 Audit
			if (indexedAudits.size() > 2) {
				for (Bean auditBeingProcessed : audits) {
					// Truncate Audit record if it is the audit being processed or an audit prior and it is before the penultimate
					// Audit record
					Long processedAuditMillis = (Long) Binder.get(auditBeingProcessed, Audit.millisPropertyName);
					Long auditMillis = (Long) Binder.get(audit, Audit.millisPropertyName);
					if (processedAuditMillis.longValue() <= auditMillis.longValue()
							&& processedAuditMillis.longValue() < penultimateAudit.getMillis().longValue()
							|| Operation.reconstruction.equals(Binder.get(audit, Audit.operationPropertyName))) {
						auditBizIdsToTruncate.add(auditBeingProcessed.getBizId());
					}
				}
			}
		}
		return auditBizIdsToTruncate;
	}
	
	/**
	 * Helper method for the TruncateAuditLogJob
	 * 
	 * Truncates the 100 audits passed in and deletes through SQL database connection
	 * 
	 * @param pers
	 * @param auditsToTruncate
	 */
	public static void truncateAuditBatch(Persistence pers, List<String> bizIdBatch) {
		SQL sql = pers.newSQL("delete from ADM_Audit where bizId in (:bizIdBatch)");
		sql.putParameter("bizIdBatch", bizIdBatch, AttributeType.id);
		sql.execute();
	}
}
