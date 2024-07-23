package modules.admin.Tag;

import java.util.ArrayList;
import java.util.List;

import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

public enum TagDefaultAction {
	tagUpsert("SkyveUpsert", "Upsert (Save Top Level Only) Documents"), tagResave("SkyveResave", "Save Documents"), tagDelete("SkyveDelete", "Delete Documents"), tagValidate("SkyveValidate", "Validate Documents");

	private String code;
	private String description;

	/** @hidden */
	private static List<DomainValue> domainValues;

	private TagDefaultAction(String code, String description) {
		this.code = code;
		this.description = description;
	}

	public String toCode() {
		return code;
	}

	public String toLocalisedDescription() {
		return Util.i18n(description);
	}

	public static TagDefaultAction fromCode(String code) {
		TagDefaultAction result = null;

		for (TagDefaultAction value : values()) {
			if (value.code.equals(code)) {
				result = value;
				break;
			}
		}

		return result;
	}

	public static TagDefaultAction fromLocalisedDescription(String description) {
		TagDefaultAction result = null;

		for (TagDefaultAction value : values()) {
			if (value.toLocalisedDescription().equals(description)) {
				result = value;
				break;
			}
		}

		return result;
	}

	public static List<DomainValue> toDomainValues() {
		if (domainValues == null) {
			TagDefaultAction[] values = values();
			domainValues = new ArrayList<>(values.length);
			for (TagDefaultAction value : values) {
				domainValues.add(new DomainValue(value.code, value.description));
			}
		}

		return domainValues;
	}
	
	public static boolean isDefaultTagAction(String actionName){
		boolean defaultAction = false;
		for(TagDefaultAction da: TagDefaultAction.values()){
			if(da.equals(TagDefaultAction.fromCode(actionName))){
				defaultAction= true;
				break;
			}
		}
		return defaultAction;
	}
}