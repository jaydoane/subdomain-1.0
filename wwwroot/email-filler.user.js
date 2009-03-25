// Email filler user script
// version 0.1
// 2009-03-07
// Copyright (c) 2009, Jay Doane
// Released under the GPL license
// http://www.gnu.org/copyleft/gpl.html
//
// ==UserScript==
// @name          Email Filler
// @namespace     http://m82.com/projects/emailfill/
// @description   Fills email textboxes with auto generated email addresses
// @include       *
// ==/UserScript==

var resthost = "m82.com";
if (document.location.hostname == "localhost") {
    // assume development configuration - will likely break using ssl tunnels
    resthost = "localhost";
 }

/**
*
*  Secure Hash Algorithm (SHA1)
*  http://www.webtoolkit.info/
*
**/
 
function sha1(msg) {
 
	function rotate_left(n,s) {
		var t4 = ( n<<s ) | (n>>>(32-s));
		return t4;
	}
 
	function lsb_hex(val) {
		var str="";
		var i;
		var vh;
		var vl;
 
		for( i=0; i<=6; i+=2 ) {
			vh = (val>>>(i*4+4))&0x0f;
			vl = (val>>>(i*4))&0x0f;
			str += vh.toString(16) + vl.toString(16);
		}
		return str;
	}
 
	function cvt_hex(val) {
		var str="";
		var i;
		var v;
 
		for( i=7; i>=0; i-- ) {
			v = (val>>>(i*4))&0x0f;
			str += v.toString(16);
		}
		return str;
	}
 
 
	function Utf8Encode(string) {
		string = string.replace(/\r\n/g,"\n");
		var utftext = "";
 
		for (var n = 0; n < string.length; n++) {
 
			var c = string.charCodeAt(n);
 
			if (c < 128) {
				utftext += String.fromCharCode(c);
			}
			else if((c > 127) && (c < 2048)) {
				utftext += String.fromCharCode((c >> 6) | 192);
				utftext += String.fromCharCode((c & 63) | 128);
			}
			else {
				utftext += String.fromCharCode((c >> 12) | 224);
				utftext += String.fromCharCode(((c >> 6) & 63) | 128);
				utftext += String.fromCharCode((c & 63) | 128);
			}
 
		}
 
		return utftext;
	}
 
	var blockstart;
	var i, j;
	var W = new Array(80);
	var H0 = 0x67452301;
	var H1 = 0xEFCDAB89;
	var H2 = 0x98BADCFE;
	var H3 = 0x10325476;
	var H4 = 0xC3D2E1F0;
	var A, B, C, D, E;
	var temp;
 
	msg = Utf8Encode(msg);
 
	var msg_len = msg.length;
 
	var word_array = new Array();
	for( i=0; i<msg_len-3; i+=4 ) {
		j = msg.charCodeAt(i)<<24 | msg.charCodeAt(i+1)<<16 |
		msg.charCodeAt(i+2)<<8 | msg.charCodeAt(i+3);
		word_array.push( j );
	}
 
	switch( msg_len % 4 ) {
		case 0:
			i = 0x080000000;
		break;
		case 1:
			i = msg.charCodeAt(msg_len-1)<<24 | 0x0800000;
		break;
 
		case 2:
			i = msg.charCodeAt(msg_len-2)<<24 | msg.charCodeAt(msg_len-1)<<16 | 0x08000;
		break;
 
		case 3:
			i = msg.charCodeAt(msg_len-3)<<24 | msg.charCodeAt(msg_len-2)<<16 | msg.charCodeAt(msg_len-1)<<8	| 0x80;
		break;
	}
 
	word_array.push( i );
 
	while( (word_array.length % 16) != 14 ) word_array.push( 0 );
 
	word_array.push( msg_len>>>29 );
	word_array.push( (msg_len<<3)&0x0ffffffff );
 
 
	for ( blockstart=0; blockstart<word_array.length; blockstart+=16 ) {
 
		for( i=0; i<16; i++ ) W[i] = word_array[blockstart+i];
		for( i=16; i<=79; i++ ) W[i] = rotate_left(W[i-3] ^ W[i-8] ^ W[i-14] ^ W[i-16], 1);
 
		A = H0;
		B = H1;
		C = H2;
		D = H3;
		E = H4;
 
		for( i= 0; i<=19; i++ ) {
			temp = (rotate_left(A,5) + ((B&C) | (~B&D)) + E + W[i] + 0x5A827999) & 0x0ffffffff;
			E = D;
			D = C;
			C = rotate_left(B,30);
			B = A;
			A = temp;
		}
 
		for( i=20; i<=39; i++ ) {
			temp = (rotate_left(A,5) + (B ^ C ^ D) + E + W[i] + 0x6ED9EBA1) & 0x0ffffffff;
			E = D;
			D = C;
			C = rotate_left(B,30);
			B = A;
			A = temp;
		}
 
		for( i=40; i<=59; i++ ) {
			temp = (rotate_left(A,5) + ((B&C) | (B&D) | (C&D)) + E + W[i] + 0x8F1BBCDC) & 0x0ffffffff;
			E = D;
			D = C;
			C = rotate_left(B,30);
			B = A;
			A = temp;
		}
 
		for( i=60; i<=79; i++ ) {
			temp = (rotate_left(A,5) + (B ^ C ^ D) + E + W[i] + 0xCA62C1D6) & 0x0ffffffff;
			E = D;
			D = C;
			C = rotate_left(B,30);
			B = A;
			A = temp;
		}
 
		H0 = (H0 + A) & 0x0ffffffff;
		H1 = (H1 + B) & 0x0ffffffff;
		H2 = (H2 + C) & 0x0ffffffff;
		H3 = (H3 + D) & 0x0ffffffff;
		H4 = (H4 + E) & 0x0ffffffff;
 
	}
 
	var temp = cvt_hex(H0) + cvt_hex(H1) + cvt_hex(H2) + cvt_hex(H3) + cvt_hex(H4);
 
	return temp.toLowerCase();
 
}

function trim_leading_www(domain) {
    var toks = domain.split(".");
    if (toks.length > 1 && toks[0] == 'www') {
        toks = toks.slice(1);
    }
    return toks.join(".");
}

// thanks to http://www.jonasjohn.de/
function create_button(id, title){
    var style = 'border:1px solid; border-color:#FC9 #630 #330 #F96; padding:1px 4px 1px 4px;' + 
        'font:bold 8px verdana,sans-serif;color:#FFF;background:#F60;' + 
        'text-decoration:none; margin: 0px;';
    return ' <a id="'+id+'" href="javascript:void(0)" style="'+style+'">'+title+'</a>';
}

function get_subdom(host) {
    var subdom = GM_getValue('subdomain');
    if (!subdom) {
        subdom = prompt("What is your " + host + " subdomain?");
        if (subdom.split(".").length < 2) {
            subdom = subdom + '.' + host;
        }
        GM_setValue('subdomain', subdom);
    }
    return subdom;
}

function get_shapass(host) {
    var shapass = GM_getValue('shapass');
    if (!shapass) {
        shapass = sha1(prompt("What is your " + host + " password?"));
        GM_setValue('shapass', shapass);
    }
    return shapass;
}

function is_empty_identifiable_email_textbox(input) {
    if (input.type == 'text' && input.value.length < 1) {
        var id = input.id.toLowerCase().replace('_', ''); // 'e_mail' ok
        if (id.indexOf('email') > -1) {
            return true;
        }
        if (id.indexOf('email') > -1 &&
            document.getElementsByName(input.name).length == 1)
            return true;
    }
    return false;
}

function filter_array(nodes, filter) {
    var result = [];
    for (var i=0; i<nodes.length; i++) {
        var node = nodes[i];
        if (filter(node)) {
            result.push(node);
        }
    }
    return result;
}

var domain = trim_leading_www(document.domain);

var email_textboxes = filter_array(document.getElementsByTagName('input'),
                                   is_empty_identifiable_email_textbox);
console.log(email_textboxes);

for (var i=0; i<email_textboxes.length; i++) (function(textbox){
        //var textbox = email_textboxes[i];
        console.log(textbox);
        var subdom = get_subdom(resthost);
        var shapass = get_shapass(resthost);

        var create_alias = function(event) {
            var alias = prompt("Create alias?", domain + '@' + subdom);
            if (alias) {
                var resturl = 'http://' + resthost + ':7000/alias';
                GM_xmlhttpRequest({
                    method: 'POST',
                            url: resturl,
                            data: shapass + " " + escape(alias),
                            onload: function(response) {
                            if (response.status == 200 || response.status == 201) {
                                console.log(textbox);
                                console.log(response);
                                document.getElementById('gen_btn'+textbox.id).style.display = 'none';
                                //textbox.value = response.responseText; // why doesn't this work?
                                var tb;
                                if (textbox.id) {
                                    tb = document.getElementById(textbox.id);
                                } else if (textbox.name) {
                                    tb = document.getElementsByName(textbox.name)[0];
                                } else {
                                    console.error('could not get textbox to fill')
                                        } 
                                tb.value = response.responseText;
                            } else {
                                alert('Alias not created: ' + ' ' + response.statusText +
                                      '\n' + response.responseText);
                            }
                        },
                            onerror: function(response) {
                            alert('Alias creation error: ' + response.status +
                                  ' ' + response.statusText + '\n\n' +
                                  'data:\n' + response.responseText);
                        }
                    });
            }
        };
        var gen_btn = create_button('gen_btn'+textbox.id, 'use&nbsp;' + resthost + '&nbsp;alias');
        console.log(gen_btn);
        textbox.parentNode.innerHTML += "&nbsp;" + gen_btn;
        document.getElementById('gen_btn'+textbox.id).addEventListener('click', create_alias, false);
    })(email_textboxes[i]);

function insert_auth_panel() {
    var auth_panel = 
        (<r><![CDATA[
                     <div id='subd_auth_div' style='padding: 3px; border: 1px solid #111111'> 
                     <form>
                     <span>activate email-filler: &nbsp;&nbsp;domain</span> 
                     <input id='subd_domain' type='textbox'/> 
                     <span>.m82.com&nbsp;&nbsp;&nbsp;password</span> 
                     <input id='subd_pass' type='password'/> 
                     <input id='subd_auth_btn' type='button' value='authenticate'/>
                     </form>
                     </div>
                     ]]></r>).toString();
    document.body.innerHTML = auth_panel + document.body.innerHTML;
}

function auth_user(event) {
    document.getElementById('subd_auth_div').style.display = 'none';
}

if (email_textboxes.length > 0) {
    insert_auth_panel();
    document.getElementById('subd_auth_btn').addEventListener('click', auth_user, false);
 }
