package modules.admin.ControlPanel;

import java.util.Stack;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class test {

	
	public static String replaceGroup(String regex, String source, int groupToReplace, String replacement) {
	    return replaceGroup(regex, source, groupToReplace, 1, replacement);
	}

	public static String replaceGroup(String regex, String source, int groupToReplace, int groupOccurrence, String replacement) {
	    Matcher m = Pattern.compile(regex).matcher(source);
	    for (int i = 0; i < groupOccurrence; i++)
	        if (!m.find()) return source; // pattern not met, may also throw an exception here
	    return new StringBuilder(source).replace(m.start(groupToReplace), m.end(groupToReplace), replacement).toString();
	}

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		String example=  "// bla bla \nurl:{ \n    testRecipient: \"test@tst.com\" \n}\napi: { \n\tbla: \"bla\" \n\t \n\tslack: { \n\t\ttoken: \"sksdksdksdkj\", \n\t\tthingo: \"blablabla\" \n\t }, \n\t//This is an example of testRecipient: \"test@tst.com\" \n\t testRecipient:   \"test@tst.com\", \n\tx: \"bla\", \n\tserver:   \"test@tst.com\" \n\t} \n something: { }";
//		String example=  "bla bla \n  url:{ \n    testRecipient: \"test@tst.com\" \n}\n api: { } \n something: { }";
		String repExp ="api:\\s*(.|\\n)*?\\n\\s*slack:\\s*(.|\\n)*?\\n\\s*token:\\s*(\"sksdksdksdkj\")";
//		String repExp ="api:\\s*\\{(\\n|\\s)*?\\}token:\\s*(\\{)(\\n|\\s)*?(\\})";
		String replacement = "\"mytoken\"";
		System.out.println(example);
//		System.out.println(repExp);
		
		System.out.println();
		System.out.println();
		System.out.println(replaceGroup(repExp, example, 3, replacement));
		
		System.out.print("bla bla.bla".contains("."));
		
//		String[] parts = "smtp.server".split("\\.");
//		System.out.println(parts[0] + "." + parts[1]);
		
	}

}
