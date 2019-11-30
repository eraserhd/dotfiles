provider "aws" {
    region  = "us-west-2"
    profile = "jason.m.felice"
    version = "~> 2.40"
}

provider "aws" {
    region  = "us-east-1"
    profile = "jason.m.felice"
    version = "~> 2.40"
    alias   = "us-east-1"
}

resource "aws_route53_zone" "eraserhead_net_zone" {
    name = "eraserhead.net"
}

resource "aws_s3_bucket" "eraserhead_net_bucket" {
    bucket = "eraserhead.net"
    acl    = "public-read"
    policy = <<EOF
{
    "Version": "2012-10-17",
    "Id": "Eraserhead.net Access",
    "Statement": [
        {
            "Sid": "Public Read Access",
            "Effect": "Allow",
            "Principal": "*",
            "Action": "s3:GetObject",
            "Resource": "arn:aws:s3:::eraserhead.net/*"
        }
    ]
}
EOF
    website {
        index_document = "index.html"
        error_document = "error.html"
    }
}

resource "aws_acm_certificate" "eraserhead_net_ssl_cert" {
    provider                  = aws.us-east-1
    domain_name               = "eraserhead.net"
    subject_alternative_names = [ "*.eraserhead.net" ]
    validation_method         = "DNS"
    lifecycle {
        create_before_destroy = true
    }
}

resource "aws_cloudfront_distribution" "eraserhead_net_cloudfront" {
    origin {
        domain_name = aws_s3_bucket.eraserhead_net_bucket.bucket_regional_domain_name
        origin_id   = "defaultS3"
    }

    enabled             = true
    is_ipv6_enabled     = true
    default_root_object = "index.html"

    aliases = [ "eraserhead.net", "www.eraserhead.net", "fixme.eraserhead.net" ]

    default_cache_behavior {
        allowed_methods        = [ "GET", "HEAD" ]
        cached_methods         = [ "GET", "HEAD" ]
        target_origin_id       = "defaultS3"
        viewer_protocol_policy = "allow-all"
        min_ttl                = 0
        default_ttl            = 3600
        max_ttl                = 86400

        forwarded_values {
            query_string = false
            cookies {
                forward = "none"
            }
        }
    }

    price_class = "PriceClass_200"

    restrictions {
        geo_restriction {
            restriction_type = "none"
        }
    }

    viewer_certificate {
        acm_certificate_arn      = aws_acm_certificate.eraserhead_net_ssl_cert.arn
        minimum_protocol_version = "TLSv1.1_2016"
        ssl_support_method       = "sni-only"
    }
}

resource "aws_route53_record" "eraserhead_net" {
    zone_id = aws_route53_zone.eraserhead_net_zone.id
    name    = "eraserhead.net"
    type    = "A"
    alias {
        name                   = aws_cloudfront_distribution.eraserhead_net_cloudfront.domain_name
        zone_id                = aws_cloudfront_distribution.eraserhead_net_cloudfront.hosted_zone_id
        evaluate_target_health = false
    }
}

resource "aws_route53_record" "www_eraserhead_net" {
    zone_id = aws_route53_zone.eraserhead_net_zone.id
    name    = "www.eraserhead.net"
    type    = "CNAME"
    ttl     = 1800
    records = [ "eraserhead.net" ]
}

resource "aws_route53_record" "fixme_eraserhead_net" {
    zone_id = aws_route53_zone.eraserhead_net_zone.id
    name    = "fixme.eraserhead.net"
    type    = "CNAME"
    ttl     = 1800
    records = [ "eraserhead.net" ]
}
