package modules.admin.ReportTemplate;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.app.admin.ReportDataset.DatasetType;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.job.JobScheduler;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.Binder;
import org.skyve.web.WebContext;

import modules.admin.JobSchedule.JobScheduleBizlet;
import modules.admin.ReportDataset.ReportDatasetExtension;
import modules.admin.ReportParameter.ReportParameterExtension;
import modules.admin.User.UserBizlet;
import modules.admin.domain.ReportTemplate;
import modules.admin.domain.ReportTemplate.ReportType;

public class ReportTemplateBizlet extends Bizlet<ReportTemplateExtension> {

	public static final String FREEMARKER_HTML_TEMPLATE_EXTENSION = "ftlh";

	// scheduling constants
	private static final String ALL_CODE = "*";
	private static final Integer ALL_CODE_SPEC = Integer.valueOf(99);
	private static final String SELECTED_CODE = "X";
	private static final String LAST_DAY_CODE = "L";
	private static final String LAST_WEEK_DAY_CODE = "LW";
	private static final String ANY_CODE = "?";
	private static final Integer ANY_CODE_SPEC = Integer.valueOf(98);

	@Override
	public List<DomainValue> getConstantDomainValues(String attributeName) throws Exception {

		// list of modules
		if (ReportTemplate.moduleNamePropertyName.equals(attributeName)
				|| ReportTemplate.generateModuleNamePropertyName.equals(attributeName)) {
			return CORE.getUser().getCustomer().getModules().stream()
					.map(m -> new DomainValue(m.getName(), m.getTitle()))
					.collect(Collectors.toList());
		} else if (ReportTemplate.allHoursPropertyName.equals(attributeName) ||
				ReportTemplate.allMonthsPropertyName.equals(attributeName) ||
				ReportTemplate.allWeekdaysPropertyName.equals(attributeName)) {
			List<DomainValue> result = new ArrayList<>(2);
			result.add(new DomainValue(ALL_CODE, "All"));
			result.add(new DomainValue(SELECTED_CODE, "Selected"));

			return result;
		} else if (ReportTemplate.allDaysPropertyName.equals(attributeName)) {
			List<DomainValue> result = new ArrayList<>(4);
			result.add(new DomainValue(ALL_CODE, "All"));
			result.add(new DomainValue(LAST_DAY_CODE, "Last Day"));
			result.add(new DomainValue(LAST_WEEK_DAY_CODE, "Last Week Day"));
			result.add(new DomainValue(SELECTED_CODE, "Selected"));

			return result;
		}

		return super.getConstantDomainValues(attributeName);
	}

	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, ReportTemplateExtension bean) throws Exception {
		// list documents within modules
		if (ReportTemplate.documentNamePropertyName.equals(attributeName)) {
			if (bean.getModuleName() != null) {
				return getDocumentsForModule(bean.getModuleName());
			}
			return Collections.emptyList();
		} else if (ReportTemplate.generateDocumentNamePropertyName.equals(attributeName)) {
			if (bean.getGenerateModuleName() != null) {
				return getDocumentsForModule(bean.getGenerateModuleName());
			}
			return Collections.emptyList();
		}

		return super.getDynamicDomainValues(attributeName, bean);
	}

	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {
		if (ReportTemplate.restrictToRolePropertyName.equals(attributeName)) {
			return UserBizlet.getCustomerRoleValues(CORE.getUser());
		}

		return super.getVariantDomainValues(attributeName);
	}

	@Override
	public ReportTemplateExtension newInstance(ReportTemplateExtension bean) throws Exception {
		ReportTemplateExtension template = super.newInstance(bean);
		template.setAllHours(ALL_CODE);
		template.setAllDays(ALL_CODE);
		template.setAllMonths(ALL_CODE);
		template.setAllWeekdays(ALL_CODE);

		return template;
	}

	@Override
	public ReportTemplateExtension preExecute(ImplicitActionName actionName, ReportTemplateExtension bean,
			Bean parentBean, WebContext webContext) throws Exception {

		if (ImplicitActionName.Edit.equals(actionName)) {
			if (bean.getAllHours() == null) {
				bean.setAllHours(ALL_CODE);
			}
			if (bean.getAllDays() == null) {
				bean.setAllDays(ALL_CODE);
			}
			if (bean.getAllMonths() == null) {
				bean.setAllMonths(ALL_CODE);
			}
			if (bean.getAllWeekdays() == null) {
				bean.setAllWeekdays(ALL_CODE);
			}
			bean.originalValues().clear();

			// populate the edit users to email collection
			bean.restoreScheduledUsers();
		}

		return super.preExecute(actionName, bean, parentBean, webContext);
	}

	@Override
	public void preRerender(String source, ReportTemplateExtension bean, WebContext webContext) throws Exception {
		if (ReportTemplate.reportTypePropertyName.equals(source)) {
			// clear generate/existing when the report type changes
			bean.setGenerateExisting(null);
			bean.setGenerateModuleName(null);
			bean.setGenerateDocumentName(null);
		} else if (ReportTemplate.scheduledPropertyName.equals(source)) {
			// check if this report can be scheduled
			if (Boolean.TRUE.equals(bean.getScheduled())) {
				if (bean.hasRequiredParameters()) {
					throw new ValidationException(new Message("This report has required parameters and cannot be scheduled"));
				}
			}
		}

		super.preRerender(source, bean, webContext);
	}

	@Override
	public void postLoad(ReportTemplateExtension bean) throws Exception {
		super.postLoad(bean);

		if (StringUtils.isNotBlank(bean.getCronExpression())) {
			JobScheduleBizlet.JobCronExpression expression = new JobScheduleBizlet.JobCronExpression(bean.getCronExpression());

			Set<Integer> hours = expression.getHours();
			Set<Integer> days = expression.getDaysOfMonth();
			Set<Integer> months = expression.getMonths();
			Set<Integer> weekdays = expression.getDaysOfWeek();

			if (hours.contains(ALL_CODE_SPEC)) {
				bean.setAllHours(ALL_CODE);
			} else {
				bean.setAllHours(SELECTED_CODE);
				for (int i = 0, l = 24; i < l; i++) {
					Binder.set(bean, "hour" + i, hours.contains(Integer.valueOf(i)) ? Boolean.TRUE : Boolean.FALSE);
				}
			}

			if (days.contains(ALL_CODE_SPEC) | days.contains(ANY_CODE_SPEC)) {
				bean.setAllDays(ALL_CODE);
			} else if (expression.getLastDayOfMonth()) {
				if (expression.getNearestWeekday()) {
					bean.setAllDays(LAST_WEEK_DAY_CODE);
				} else {
					bean.setAllDays(LAST_DAY_CODE);
				}
			} else {
				bean.setAllDays(SELECTED_CODE);
				for (int i = 1, l = 32; i < l; i++) {
					Binder.set(bean, "day" + i, days.contains(Integer.valueOf(i)) ? Boolean.TRUE : Boolean.FALSE);
				}
			}

			if (months.contains(ALL_CODE_SPEC)) {
				bean.setAllMonths(ALL_CODE);
			} else {
				bean.setAllMonths(SELECTED_CODE);
				for (int i = 1, l = 13; i < l; i++) {
					Binder.set(bean, "month" + i, months.contains(Integer.valueOf(i)) ? Boolean.TRUE : Boolean.FALSE);
				}
			}

			if (weekdays.contains(ALL_CODE_SPEC) || weekdays.contains(ANY_CODE_SPEC)) {
				bean.setAllWeekdays(ALL_CODE);
			} else {
				bean.setAllWeekdays(SELECTED_CODE);
				for (int i = 1, l = 8; i < l; i++) {
					Binder.set(bean, "weekday" + i, weekdays.contains(Integer.valueOf(i)) ? Boolean.TRUE : Boolean.FALSE);
				}
			}
		}
	}

	@Override
	public void preDelete(ReportTemplateExtension bean) throws Exception {
		super.preDelete(bean);
		if (UtilImpl.JOB_SCHEDULER && Boolean.TRUE.equals(bean.getScheduled())) {
			EXT.getJobScheduler().unscheduleReport(bean, CORE.getUser().getCustomer());
		}
	}

	@Override
	public void preSave(ReportTemplateExtension bean) throws Exception {
		JobScheduler jobScheduler = EXT.getJobScheduler();
		
		// update the templateName if not set or needs to be changed
		if (bean.getTemplateName() == null || bean.originalValues().containsKey(ReportTemplate.namePropertyName)) {
			bean.setTemplateName(String.format("%s.%s", bean.getName(), FREEMARKER_HTML_TEMPLATE_EXTENSION));
		}

		if (UtilImpl.JOB_SCHEDULER && (bean.isNotPersisted() || bean.originalValues().containsKey(ReportTemplate.namePropertyName))) {
			jobScheduler.addReportJob(bean.getName());
		}

		// update the scheduling if enabled
		final Customer customer = CORE.getCustomer();
		if (Boolean.TRUE.equals(bean.getScheduled())) {
			if (bean.getRunAs() == null) {
				throw new ValidationException(ReportTemplate.runAsPropertyName,
						"Please specify the user to run the report as.");
			}

			StringBuilder expression = new StringBuilder(128);

			// append seconds
			expression.append("0 0 ");

			// append hours
			if (bean.getAllHours() == null || ALL_CODE.equals(bean.getAllHours())) {
				expression.append(ALL_CODE);
			} else {
				for (int i = 0, l = 24; i < l; i++) {
					if (Boolean.TRUE.equals(Binder.get(bean, "hour" + i))) {
						expression.append(i).append(',');
					}
				}
				expression.setLength(expression.length() - 1); // remove last comma
			}
			expression.append(' ');

			// append days
			String allDays = bean.getAllDays();
			String allWeekdays = bean.getAllWeekdays();
			if (bean.getAllDays() == null || ALL_CODE.equals(allDays)) {
				if (SELECTED_CODE.equals(allWeekdays)) {
					expression.append(ANY_CODE);
				} else {
					expression.append(ALL_CODE);
				}
			} else if (LAST_DAY_CODE.equals(allDays)) {
				expression.append(LAST_DAY_CODE);
			} else if (LAST_WEEK_DAY_CODE.equals(allDays)) {
				expression.append(LAST_WEEK_DAY_CODE);
			} else {
				for (int i = 1, l = 32; i < l; i++) {
					if (Boolean.TRUE.equals(Binder.get(bean, "day" + i))) {
						expression.append(i).append(',');
					}
				}
				expression.setLength(expression.length() - 1); // remove last comma
			}
			expression.append(' ');

			// append months
			if (bean.getAllMonths() == null || ALL_CODE.equals(bean.getAllMonths())) {
				expression.append(ALL_CODE);
			} else {
				for (int i = 1, l = 13; i < l; i++) {
					if (Boolean.TRUE.equals(Binder.get(bean, "month" + i))) {
						expression.append(i).append(',');
					}
				}
				expression.setLength(expression.length() - 1); // remove last comma
			}
			expression.append(' ');

			// append weekdays
			if (bean.getAllWeekdays() == null || ALL_CODE.equals(bean.getAllWeekdays())) {
				expression.append(ANY_CODE);
			} else {
				for (int i = 1, l = 8; i < l; i++) {
					if (Boolean.TRUE.equals(Binder.get(bean, "weekday" + i))) {
						expression.append(i).append(',');
					}
				}
				expression.setLength(expression.length() - 1); // remove last comma
			}

			bean.setCronExpression(expression.toString());

			if (UtilImpl.JOB_SCHEDULER) {
				// Re-schedule the job
				jobScheduler.unscheduleReport(bean, customer);

				// Determine the job schedule user
				StringBuilder userPrincipal = new StringBuilder(128);
				userPrincipal.append(customer.getName());
				userPrincipal.append('/').append(bean.getRunAs().getUserName());
				User user = CORE.getRepository().retrieveUser(userPrincipal.toString());
				jobScheduler.scheduleReport(bean, user);
			}
		} else if (UtilImpl.JOB_SCHEDULER && Boolean.TRUE.equals(bean.originalValues().get(ReportTemplate.scheduledPropertyName))) {
			jobScheduler.unscheduleReport(bean, customer);
		}

		// populate the edit users to email collection
		if (bean.originalValues().containsKey(ReportTemplate.editUsersToEmailPropertyName)) {
			bean.updateScheduledUsers();
		}

		super.preSave(bean);
	}

	@Override
	public void validate(ReportTemplateExtension bean, ValidationException e) throws Exception {
		super.validate(bean, e);

		if (Boolean.TRUE.equals(bean.getScheduled())) {
			// check if there are any required parameters
			if (bean.hasRequiredParameters()) {
				e.getMessages().add(new Message("This report has required parameters and cannot be scheduled"));
			}

			if (bean.getUsersToEmail().isEmpty()) {
				e.getMessages().add(new Message(ReportTemplate.usersToEmailPropertyName,
						"Please provide at least one user to email the report to"));
			}

			if ((bean.getAllDays() != null && !bean.getAllDays().equals(ALL_CODE))
					&& (bean.getAllWeekdays() != null && !bean.getAllWeekdays().equals(ALL_CODE))) {
				e.getMessages().add(new Message(
						new String[] { ReportTemplate.allDaysPropertyName, ReportTemplate.allWeekdaysPropertyName },
						"Choose week days or days of the month, but not both"));
			}

			if (SELECTED_CODE.equals(bean.getAllHours())) {
				boolean found = false;
				for (int i = 0, l = 24; i < l; i++) {
					if (Boolean.TRUE.equals(Binder.get(bean, "hour" + i))) {
						found = true;
						break;
					}
				}
				if (!found) {
					e.getMessages().add(new Message(ReportTemplate.allHoursPropertyName, "Must select at least one hour."));
				}
			}
			if (SELECTED_CODE.equals(bean.getAllDays())) {
				boolean found = false;
				for (int i = 1, l = 32; i < l; i++) {
					if (Boolean.TRUE.equals(Binder.get(bean, "day" + i))) {
						found = true;
						break;
					}
				}
				if (!found) {
					e.getMessages().add(new Message(ReportTemplate.allDaysPropertyName, "Must select at least one day."));
				}
			}
			if (SELECTED_CODE.equals(bean.getAllMonths())) {
				boolean found = false;
				for (int i = 1, l = 13; i < l; i++) {
					if (Boolean.TRUE.equals(Binder.get(bean, "month" + i))) {
						found = true;
						break;
					}
				}
				if (!found) {
					e.getMessages().add(new Message(ReportTemplate.allMonthsPropertyName, "Must select at least one month."));
				}
			}
			if (SELECTED_CODE.equals(bean.getAllWeekdays())) {
				boolean found = false;
				for (int i = 1, l = 8; i < l; i++) {
					if (Boolean.TRUE.equals(Binder.get(bean, "weekday" + i))) {
						found = true;
						break;
					}
				}
				if (!found) {
					e.getMessages()
							.add(new Message(ReportTemplate.allWeekdaysPropertyName, "Must select at least one week day."));
				}
			}
		}

		validateReportParameters(bean, e);
	}

	/**
	 * Returns a list of document name domain values for the specified module name.
	 */
	private static List<DomainValue> getDocumentsForModule(final String moduleName) {
		List<DomainValue> results = new ArrayList<>();
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(moduleName);
		for (String documentName : module.getDocumentRefs().keySet()) {
			Document document = module.getDocument(customer, documentName);
			results.add(new DomainValue(document.getName(), document.getSingularAlias()));
		}
		// sort the list by description in case the singular alias changes the sort order
		results.sort(Comparator.comparing(DomainValue::getLocalisedDescription));

		return results;
	}

	/**
	 * Validates that all ReportParameters for this template are used by at least one ReportDataset query.
	 * 
	 * @param bean The ReportTemplate to validate
	 * @param e The ValidationException any errors will be added to
	 */
	private static void validateReportParameters(ReportTemplateExtension bean, ValidationException e) {
		// skip validation for jasper reports
		if (bean.getReportType() == ReportType.jasper) {
			return;
		}

		for (ReportParameterExtension param : bean.getParameters()) {
			boolean inUse = false;

			for (ReportDatasetExtension dataset : bean.getDatasets()) {
				// skip constants as they can't accept parameters
				if (dataset.getDatasetType() == DatasetType.constant) {
					continue;
				}

				// class datasets always inject all parameters
				if (dataset.getDatasetType() == DatasetType.classValue) {
					inUse = true;
					break;
				}

				// check bizQL or SQL datasets for all parameters
				if (dataset.getDatasetType() == DatasetType.bizQL || dataset.getDatasetType() == DatasetType.SQL) {
					if (dataset.containsParameter(param)) {
						inUse = true;
						break;
					}
				}
			}

			if (inUse == false) {
				e.getMessages()
						.add(new Message(String.format(
								"Parameter %s is not in use by any dataset. Please include it in a dataset or remove it.",
								param.getName())));
			}
		}
	}
}
