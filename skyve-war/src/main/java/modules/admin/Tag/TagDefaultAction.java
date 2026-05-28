package modules.admin.Tag;

import java.util.ArrayList;
import java.util.List;

import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

/**
 * Enumerates built-in actions that can be executed against a tag selection.
 */
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

	/**
	 * Returns the persisted code for this default action.
	 *
	 * @return The stable action code.
	 */
	public String toCode() {
		return code;
	}

	/**
	 * Returns the localised description for UI rendering.
	 *
	 * @return The localised action description.
	 */
	public String toLocalisedDescription() {
		return Util.i18n(description);
	}

	/**
	 * Resolves a default action from its persisted code.
	 *
	 * @param code The action code to resolve.
	 * @return Matching enum constant, or {@code null} when unknown.
	 */
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

	/**
	 * Resolves a default action from its localised description.
	 *
	 * @param description The localised description to resolve.
	 * @return Matching enum constant, or {@code null} when not found.
	 */
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

	/**
	 * Converts all enum values into domain values for metadata dropdowns.
	 *
	 * @return Cached list of code/description pairs.
	 */
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
	
	/**
	 * Indicates whether a supplied action name maps to a built-in tag action.
	 *
	 * @param actionName The action name to test.
	 * @return {@code true} when actionName is one of the built-in codes.
	 */
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