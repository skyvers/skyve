package modules.admin.ReportDataset;

import org.skyve.domain.app.admin.ReportParameter.Type;
import org.skyve.util.Binder;

import jakarta.enterprise.inject.Default;
import modules.admin.ReportParameter.ReportParameterExtension;
import modules.admin.domain.ReportParameter;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient ReportDatasetService reportDatasetService;
 */
@Default
public class ReportDatasetService {
	/**
	 * Creates a ReportParameterExtension for the supplied parameter name, and automatically
	 * guesses the parameter type of date if the name ends with "date".
	 * 
	 * @param parameterName The name of the new parameter
	 * @return The new parameter with the name and description defaulted
	 */
	@SuppressWarnings("static-method")
	protected ReportParameterExtension createNewParameter(String parameterName) {
		ReportParameterExtension newParam = ReportParameter.newInstance();
		newParam.setName(parameterName);
		newParam.setDescription(Binder.toTitleCase(parameterName));

		// if the parameter name ends with "date", presume the type to be date
		if (parameterName.endsWith("Date")) {
			newParam.setType(Type.date);
		}

		return newParam;
	}
}
