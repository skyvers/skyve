package modules.admin.Audit;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.domain.Audit;

/**
 * Coordinates the audit form lifecycle for the admin module.
 *
 * <p>The bizlet reacts to edit-view selection changes by loading the available
 * audit versions for comparison and by resolving archived audit records when a
 * user opens an archived audit instance.
 */
public class AuditBizlet extends Bizlet<Audit> {
	@Inject
	@SuppressWarnings("java:S6813") // allow member injection
	private transient AuditService auditService;

	/**
	 * Initialises the audit bean before the selected action executes.
	 *
	 * <p>When the edit action opens an audit record, the current bean is copied
	 * into the source-version field so that the comparison selector can resolve the
	 * correct historical versions.
	 *
	 * @param actionName the implicit action being executed; only {@link ImplicitActionName#Edit}
	 *                   triggers custom behaviour
	 * @param bean the audit bean being edited; never {@code null}
	 * @param parentBean the owning bean, or {@code null} when the audit has no parent
	 * @param webContext the current web context; never {@code null}
	 * @return the bean to continue editing; never {@code null}
	 */
	@Override
	public Audit preExecute(ImplicitActionName actionName, Audit bean, Bean parentBean, WebContext webContext) {
		if (ImplicitActionName.Edit.equals(actionName)) {
			bean.setSourceVersion(bean);
			auditService.sourceVersionChanged(bean);
		}

		return bean;
	}

	/**
	 * Refreshes derived audit state after a field change and before the view rerenders.
	 *
	 * <p>When the source-version selector changes, the available comparison versions
	 * are recalculated to keep the second selector in sync with the new source.
	 *
	 * @param source the binding that changed; never {@code null}
	 * @param bean the audit bean being rerendered; never {@code null}
	 * @param webContext the current web context; never {@code null}
	 * @throws Exception if the audit service cannot recompute the version list
	 */
	@Override
	public void preRerender(String source, Audit bean, WebContext webContext) throws Exception {
		if (Audit.sourceVersionPropertyName.equals(source)) {
			auditService.sourceVersionChanged(bean);
		}

		super.preRerender(source, bean, webContext);
	}

	/**
	 * Supplies the dynamic audit-version choices for the comparison selectors.
	 *
	 * <p>The source-version selector clears the bean's original values before
	 * returning all available versions. The comparison-version selector only
	 * returns versions for the currently selected source version.
	 *
	 * @param attributeName the attribute being populated; never {@code null}
	 * @param bean the audit bean providing the selection context; never {@code null}
	 * @return the domain values for the selector, or {@code null} for unrelated attributes
	 */
	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, Audit bean) {
		if (Audit.sourceVersionPropertyName.equals(attributeName)) {
			bean.originalValues()
					.clear();
			return auditService.getVersions(bean, false);
		} else if (Audit.comparisonVersionPropertyName.equals(attributeName) &&
				(bean.getSourceVersion() != null)) {
			return auditService.getVersions(bean.getSourceVersion(), true);
		}

		return null;
	}

	/**
	 * Resolves an archived audit bean by business identifier.
	 *
	 * <p>The archive store is consulted first because archived audit records may no
	 * longer exist in the relational database. If the archive lookup fails, the
	 * method returns {@code null} and the caller continues with the normal flow.
	 *
	 * @param bizId the business identifier of the audit record to resolve; never {@code null}
	 * @param conversationBean the current conversation bean, ignored by this resolver
	 * @param webContext the current web context, ignored by this resolver
	 * @return the archived audit record, or {@code null} if it cannot be resolved
	 */
	@Override
	public Audit resolve(String bizId, Bean conversationBean, WebContext webContext) {
		// Load the requested Audit instance from the archives, if available
		Audit result = auditService.retrieveFromArchives(bizId);

		if (result == null) {
			LOGGER.trace("Unabled to resolve Audit {} from archives", bizId);
		} else {
			LOGGER.trace("Resolved {} for {} from archives", result, bizId);
		}

		return result;
	}
}
