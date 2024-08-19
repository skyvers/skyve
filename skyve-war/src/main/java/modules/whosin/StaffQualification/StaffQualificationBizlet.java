package modules.whosin.StaffQualification;

import java.util.ArrayList;
import java.util.List;

import org.skyve.metadata.model.document.Bizlet;

import modules.whosin.domain.StaffQualification;

public class StaffQualificationBizlet extends Bizlet<StaffQualification> {

	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, StaffQualification bean) throws Exception {
		if (StaffQualification.levelPropertyName.equals(attributeName)) {
			List<DomainValue> values = new ArrayList<>();
			values.add(new DomainValue("first"));
			values.add(new DomainValue("second"));
			values.add(new DomainValue("third"));
			return values;
		}
		return super.getDynamicDomainValues(attributeName, bean);
	}

}
