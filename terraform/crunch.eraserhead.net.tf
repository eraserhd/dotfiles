resource "aws_route53_record" "crunch_eraserhead_net_mx" {
    zone_id  = aws_route53_zone.eraserhead_net_zone.id
    name     = "crunch.eraserhead.net"
    type     = "MX"
    ttl      = 14400
    records  = [ "10 inbound-smtp.us-west-2.amazonaws.com" ]
}

resource "aws_route53_record" "_amazonses_crunch_eraserhead_net" {
    zone_id  = aws_route53_zone.eraserhead_net_zone.id
    name     = "_amazonses.crunch.eraserhead.net"
    type     = "TXT"
    ttl      = 3600
    records  = [ "r8xzecfaQLfrrZaXnENC8vnsJsVBG2BUZ8gXLaojV04=" ]
}

resource "aws_route53_record" "crunch_eraserhead_net_aaaa" {
    zone_id  = aws_route53_zone.eraserhead_net_zone.id
    name     = "crunch"
    type     = "AAAA"
    ttl      = 3600
    records  = [
      jsondecode(file("${path.module}/../machines/crunch/ip.json")).ip
    ]
}
